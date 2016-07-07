package hanoi.veze
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.Random

@JSExport
object HanojskeVeze {


  var pocetDisku = 3
  val sirkaMinDisku = 50
  val prirustekSirky = 40
  val vyskaDisku = 20
  val mezera = 3
  var rozmeryCanvas :(Int, Int) = (0,0)
  var zapisnik:Zapisnik = null

  var tycky : Array[Tycka] =  Array(new Tycka, new Tycka, new Tycka)
  var prenasenyDisk: (Int, Disk) = (-1,null)


  case class Zapisnik(val h:html.TextArea){
    var pocetTahu=0
    h.value=""
    def zapis(jaky:String): Unit = {
      h.value = h.value+jaky
    }
    def zapisTah(od:Int, kam:Int): Unit = {
      pocetTahu+=1
      zapis(pocetTahu+". ("+od+" -> "+kam+")\n")
    }
  }

  case class Disk (val sirka: Int, val barva: String){
    def nakresliSe(pozice: (Int, Int), kam: dom.CanvasRenderingContext2D) : Unit = {
      kam.fillStyle = this.barva
      kam.fillRect(pozice._1-(sirka/2), pozice._2, sirka, vyskaDisku);
    }
  }

  class Tycka {
    var disky: List[Disk] = List()
    var pozice: (Int, Int) = _ //horni levy roh tycky
    var platno: dom.CanvasRenderingContext2D = _
    var delkaTycky=0;
    var horniMezera=0;
    vymazDisky;


    def lzeVlozit(d: Disk): Boolean = ((disky.isEmpty) || (disky.head.sirka > d.sirka))

    def +(ds: Disk): Tycka = {
      if (lzeVlozit(ds)) {
        disky = ds :: disky
        horniMezera -= (vyskaDisku+mezera)
      }
      this
    }

    def >(t: Tycka) : Boolean = {
      if (disky.isEmpty) false
      val d: Disk = disky.head
      if (t lzeVlozit d) {
        t+pop;
        //disky = disky.tail
        //horniMezera += vyskaDisku+mezera
      }
      true
    }

    def jsemPod(poz:(Int, Int)):Boolean =
      (pozice._1-sirkaMinDisku/2 < poz._1)&&(pozice._1+sirkaMinDisku/2 > poz._1)&&
      (pozice._2<poz._2)&&(pozice._2+delkaTycky>poz._2)

    def vymazDisky:Unit = {
      delkaTycky = pocetDisku * (vyskaDisku + mezera) + 2 * mezera
      disky = List();horniMezera=delkaTycky;
    }



    def pop: Disk = {
      var h: Disk = null
      if (!(disky.isEmpty)) {
        h = disky.head
        disky = disky.tail
        horniMezera += (vyskaDisku+mezera)
      }
      h
    }

    def nakresliSe: Unit = {
      println("kreslim tycku:")
      def kresliDisky(yp: Int, t: List[Disk]): Unit =
        t match {
          case List() => Unit
          case prvni :: zbytek => {
            println("kreslim disk barvy..."+prvni.barva)
            prvni.nakresliSe((pozice._1, yp), platno)
            kresliDisky(yp + vyskaDisku + mezera, zbytek)
          }
        }

      platno.fillStyle = "red"
      platno.fillRect(pozice._1 - mezera, pozice._2, 2 * mezera, delkaTycky)
      kresliDisky(pozice._2 + horniMezera, disky)
    }
  } //Tycka


  def nahodnaBarva(): String = { //nahodna barva "#ffffff"
    val r = scala.util.Random
    var ba = List.fill(3)(r.nextInt(255))
    "#"+ba.map((e) => if (e<15) "0"+Integer.toHexString(e) else Integer.toHexString(e)).mkString("")
  }

  def naberDisk(pozice:(Int, Int)): Unit = {

    println("pozice: "+pozice)
    for (i <- Range(0,3))
      if (tycky(i).jsemPod(pozice)) {
        prenasenyDisk = (i,tycky(i).pop)
        println("nasel se"+i)
        //dom.window.alert("tycka: "+i)
      }
    println(prenasenyDisk)
  }


  def pustDisk(pozice:(Int, Int)): Unit = {
    println("pozice: "+pozice)
    println(prenasenyDisk)
    if (prenasenyDisk._2!=null) {
      for (i <- Range(0, 3))
        if ((tycky(i).jsemPod(pozice))&&(tycky(i).lzeVlozit(prenasenyDisk._2))) {
          tycky(i) = tycky(i) + prenasenyDisk._2
          //tycky(i).nakresliSe
          //tycky(prenasenyDisk._1).nakresliSe
          zapisnik.zapisTah(prenasenyDisk._1,i)
          prenasenyDisk = (-1, null)
        }
      if (prenasenyDisk._2 != null) {
        //vrat zpatky
        tycky(prenasenyDisk._1) = tycky(prenasenyDisk._1) + prenasenyDisk._2
      }
      prenasenyDisk = (-1, null)
    }
  }

  def init(): Unit = {
    var nejsirsiDisk =  sirkaMinDisku + (pocetDisku-1)*prirustekSirky
    tycky.foreach((t)=>t.vymazDisky)
    for (i <- 0 until pocetDisku) {
      tycky(0) = tycky(0) + Disk(nejsirsiDisk - i*prirustekSirky, nahodnaBarva())
    }
  }

  def initCanvas(kam: dom.CanvasRenderingContext2D): Unit = {
    kam.clearRect(0,0,rozmeryCanvas._1, rozmeryCanvas._2)
    for (i <- Range(0,3)) tycky(i).nakresliSe
  }

  @JSExport
  def main(pocD: html.Select, canvas: html.Canvas, hist: html.TextArea): Unit = {

    val renderer = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    zapisnik = Zapisnik(hist)

    pocD.onchange = (e: dom.Event) => {
      println(pocD.selectedIndex)
      pocetDisku = pocD.selectedIndex+3
      init();initCanvas(renderer)
      zapisnik = Zapisnik(hist)
    }

    canvas.width = canvas.parentElement.clientWidth
    //canvas.height = canvas.parentElement.clientHeight
    canvas.height = 400;
    rozmeryCanvas = (canvas.width, canvas.height)

    renderer.fillStyle = "#f8f8f8"
    renderer.fillRect(0, 0, canvas.width, canvas.height)

    renderer.fillStyle = "black"
    //renderer.fillRect(10,20,100,20)
    init()

    tycky(0).pozice = (300, 100)
    tycky(0).platno = renderer
    tycky(1).pozice = (600, 100)
    tycky(1).platno = renderer
    tycky(2).pozice = (900, 100)
    tycky(2).platno = renderer



    //tycky(0) > tycky(1)

    initCanvas(renderer)

    /*

    tycky(0) > tycky(2)
    tycky(1) > tycky(2)
    tycky(0) > tycky(1)

    initCanvas(renderer)
    renderer.restore()
*/

    var stisk = false

    def pozMys(x:Double, y:Double) = {
      val rect = canvas.getBoundingClientRect()
      ((x - rect.left).toInt, (y - rect.top).toInt)
    }
    canvas.onmousedown = (e: dom.MouseEvent) => {
      naberDisk(pozMys(e.clientX,e.clientY));
      initCanvas(renderer);
      stisk = true
    }
    //canvas.onmousedown = (e: dom.MouseEvent) => down = true

    canvas.onmouseup = (e: dom.MouseEvent) => {
      pustDisk(pozMys(e.clientX,e.clientY));
      initCanvas(renderer);
      stisk = false
    }

    canvas.onmousemove = { (e: dom.MouseEvent) => {
      val rect = canvas.getBoundingClientRect()
      if ((stisk)&&(prenasenyDisk._2!=null)) {
        initCanvas(renderer)
        prenasenyDisk._2.nakresliSe(pozMys(e.clientX,e.clientY), renderer)
      }
    }
    }
  } //main
} //object
