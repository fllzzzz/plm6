<template>
  <div class="component-container">
    <svg width="100%" height="100%" version="1.1" xmlns="http://www.w3.org/2000/svg" class="wave">
      <defs />
      <path id="feel-the-wave" d />
    </svg>
    <svg width="100%" height="100%" version="1.1" xmlns="http://www.w3.org/2000/svg" class="wave">
      <defs />
      <path id="feel-the-wave-two" d />
    </svg>
  </div>
</template>

<script>
// TODO: 优化
import { TweenMax, Power1 } from 'gsap'
// :height="`${waveHeight[0] + 100}px`"
export default {
  name: 'Wave',
  props: {
    waveColor: {
      type: Array,
      default: () => ['rgba(255, 255, 255, .3)', 'rgba(255, 255, 255, .5)']
    },
    waveYOffset: {
      type: Number,
      default: 0
    },
    waveDuration: {
      type: Number,
      default: 3
    }
  },
  data() {
    return {
      offset: 0,
      waveOne: null,
      waveTwo: null,
      waveHeight: [0, 0],
      lastOptionOne: {},
      lastOptionTwo: {}
    }
  },
  created() {
    const height = document.documentElement.clientHeight
    this.waveHeight = [height * 0.03, height * 0.05]
    // this.waveHeight = [height * 0.9, height * 0.88]
  },
  mounted() {
    this.init()
  },
  beforeUnmount() {
    this.kill()
  },
  methods: {
    init() {
      const height = document.querySelector('body').getBoundingClientRect().height
      this.lastOptionOne = {
        height: this.waveHeight[0],
        bones: 4,
        amplitude: 30,
        color: this.waveColor[0],
        speed: 0.15,
        waveYOffsetH: this.waveYOffset * height - this.offset
      }
      this.lastOptionTwo = {
        height: this.waveHeight[1],
        bones: 3,
        amplitude: 20,
        color: this.waveColor[1],
        speed: 0.25,
        waveYOffsetH: this.waveYOffset * height - this.offset
      }
      this.waveOne = this.wavify('#feel-the-wave', this.lastOptionOne)
      this.waveTwo = this.wavify('#feel-the-wave-two', this.lastOptionTwo)
    },
    refresh() {
      const height = document.querySelector('body').getBoundingClientRect().height
      const lastOptionOne = {
        height: this.waveHeight[0],
        bones: 4,
        amplitude: 30,
        color: this.waveColor[0],
        speed: 0.15,
        waveYOffsetH: this.waveYOffset * height - this.offset
      }
      const lastOptionTwo = {
        height: this.waveHeight[1],
        bones: 3,
        amplitude: 20,
        color: this.waveColor[1],
        speed: 0.25,
        waveYOffsetH: this.waveYOffset * height - this.offset
      }

      this.waveOne.reboot(lastOptionOne, Object.assign({}, this.lastOptionOne), this.waveDuration)
      this.waveTwo.reboot(lastOptionTwo, Object.assign({}, this.lastOptionTwo), this.waveDuration)
      this.lastOptionOne = lastOptionOne
      this.lastOptionTwo = lastOptionTwo
    },
    kill() {
      this.waveOne.kill()
      this.waveTwo.kill()
      // TweenMax.killAll()
    },
    pause() {
      const el1 = document.getElementById('feel-the-wave')
      const el2 = document.getElementById('feel-the-wave-two')
      if (el1 && el2) {
        this.waveOne.pause()
        this.waveTwo.pause()
      }
    },
    play() {
      const el1 = document.getElementById('feel-the-wave')
      const el2 = document.getElementById('feel-the-wave-two')
      if (el1 && el2) {
        this.waveOne.play()
        this.waveTwo.play()
      }
    },
    /*
     *   Wavify
     *   JavaScript library to make some nice waves
     *   by peacepostman @ crezeo
     */
    wavify(wave_element, options) {
      if (typeof options === 'undefined') options = {}

      //  Options
      var settings = Object.assign(
        {},
        {
          container: options.container ? options.container : 'body',
          // Height of wave
          height: 200,
          // Amplitude of wave
          amplitude: 100,
          // Animation speed
          speed: 0.15,
          // Total number of articulation in wave
          bones: 3,
          // Color
          color: 'rgba(255,255,255, 0.20)'
        },
        options
      )

      const wave = wave_element
      let width = document.querySelector(settings.container).getBoundingClientRect().width
      let height = document.querySelector(settings.container).getBoundingClientRect().height
      // eslint-disable-next-line no-unused-vars
      let points = []
      let lastUpdate
      let totalTime = 0
      let animationInstance = false
      let tweenMaxInstance = false

      //  Allow new settings, avoid setting new container for logic purpose please :)
      //
      // eslint-disable-next-line no-unused-vars
      function rebuilSettings(params) {
        settings = Object.assign(
          {},
          {
            container: options.container ? options.container : 'body',
            // Height of wave
            height: 200,
            // Amplitude of wave
            amplitude: 100,
            // Animation speed
            speed: 0.15,
            // Total number of articulation in wave
            bones: 3,
            // Color
            color: 'rgba(255,255,255, 0.20)'
          },
          params
        )
      }

      function drawPoints(factor) {
        var points = []

        for (var i = 0; i <= settings.bones; i++) {
          var x = (i / settings.bones) * width
          var sinSeed = (factor + (i + (i % settings.bones))) * settings.speed * 100
          var sinHeight = Math.sin(sinSeed / 100) * settings.amplitude
          var yPos = Math.sin(sinSeed / 100) * sinHeight + settings.height
          points.push({ x: x, y: yPos })
        }

        return points
      }

      function drawPath(points) {
        var SVGString = 'M ' + points[0].x + ' ' + points[0].y

        var cp0 = {
          x: (points[1].x - points[0].x) / 2,
          y: points[1].y - points[0].y + points[0].y + (points[1].y - points[0].y)
        }

        SVGString +=
          ' C ' +
          cp0.x +
          ' ' +
          cp0.y +
          ' ' +
          cp0.x +
          ' ' +
          cp0.y +
          ' ' +
          points[1].x +
          ' ' +
          points[1].y

        var prevCp = cp0
        var inverted = -1

        for (var i = 1; i < points.length - 1; i++) {
          // eslint-disable-next-line no-unused-vars
          var cpLength = Math.sqrt(prevCp.x * prevCp.x + prevCp.y * prevCp.y)
          var cp1 = {
            x: points[i].x - prevCp.x + points[i].x,
            y: points[i].y - prevCp.y + points[i].y
          }

          SVGString +=
            ' C ' +
            cp1.x +
            ' ' +
            cp1.y +
            ' ' +
            cp1.x +
            ' ' +
            cp1.y +
            ' ' +
            points[i + 1].x +
            ' ' +
            points[i + 1].y
          prevCp = cp1
          inverted = -inverted
        }

        SVGString += ' L ' + width + ' ' + height
        SVGString += ' L 0 ' + height + ' Z'
        return SVGString
      }

      //  Draw function
      //
      //
      function draw() {
        var now = window.Date.now()

        if (lastUpdate) {
          var elapsed = (now - lastUpdate) / 1000
          lastUpdate = now

          totalTime += elapsed

          var factor = totalTime * Math.PI
          tweenMaxInstance = TweenMax.to(wave, settings.speed, {
            attr: {
              d: drawPath(drawPoints(factor))
            },
            ease: Power1.easeInOut
          })
        } else {
          lastUpdate = now
        }

        animationInstance = requestAnimationFrame(draw)
      }

      //  Pure js debounce function to optimize resize method
      //
      //
      function debounce(func, wait, immediate) {
        var timeout
        return () => {
          const context = this
          const args = arguments
          clearTimeout(timeout)
          timeout = setTimeout(() => {
            timeout = null
            if (!immediate) func.apply(context, args)
          }, wait)
          if (immediate && !timeout) func.apply(context, args)
        }
      }

      //  Redraw for resize with debounce
      //
      var redraw = debounce(() => {
        pause()
        points = []
        totalTime = 0
        width = document.querySelector(settings.container).getBoundingClientRect().width
        height = document.querySelector(settings.container).getBoundingClientRect().height
        lastUpdate = false
        play()
      }, 250)

      function boot() {
        if (!animationInstance) {
          tweenMaxInstance = TweenMax.set(wave, { y: settings.waveYOffsetH, attr: { fill: settings.color }})
          // tweenMaxInstance = TweenMax.set(wave, { attr: { fill: settings.color }})
          play()
          window.addEventListener('resize', redraw, { passive: false })
        }
      }

      // eslint-disable-next-line no-unused-vars
      function reboot(options, lastOptions, waveDuration) {
        kill()
        if (typeof options !== undefined) {
          rebuilSettings(options)
        }
        tweenMaxInstance = TweenMax.fromTo(wave, waveDuration, { y: lastOptions.waveYOffsetH, attr: { fill: lastOptions.color }}, { y: settings.waveYOffsetH || 0, attr: { fill: settings.color }})
        play()
        window.addEventListener('resize', redraw, { passive: false })
      }

      function play() {
        if (!animationInstance) {
          animationInstance = requestAnimationFrame(draw)
          window.addEventListener('resize', redraw, { passive: false })
        }
      }

      function pause() {
        if (animationInstance) {
          cancelAnimationFrame(animationInstance)
          animationInstance = false
          window.removeEventListener('resize', redraw)
        }
      }

      function kill() {
        if (animationInstance) {
          pause()
          tweenMaxInstance.kill()
          tweenMaxInstance = TweenMax.set(wave, {
            x: 0,
            y: 0,
            rotation: 0,
            opacity: 0,
            clearProps: 'all',
            attr: {
              d: 'M0,0',
              fill: ''
            }
          })
          animationInstance = false
        }
        window.removeEventListener('resize', redraw)
      }

      //  Boot Wavify
      //
      boot()

      return {
        reboot: reboot,
        play: play,
        pause: pause,
        kill: kill
      }
    }
  }

}
</script>

<style scoped>
.component-container {
  position: relative;
  width: 100%;
  height: 100%;
  /* transform: rotate(180deg); */
}
.wave {
  position: absolute;
  left: 0;
  right: 0;
  bottom: 0;
  z-index: 1;
}

.wave + .wave {
  z-index: 2;
}
</style>
