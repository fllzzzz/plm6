<template>
  <div class="imgDraw">
    <a
      href="javascript:void(0);"
      ref="download"
      download="picture.png"
      v-show="false"
    ></a>
      <div class="d_body" ref="body">
        <div class="board" ref="board">
        <div style="position: absolute;" :style="boardStyle">
          <canvas
            id="ctx_front"
            ref="ctx_front"
            :style="canvasStyle"
          ></canvas>
          <canvas
            id="ctx_base"
            ref="ctx_base"
            :style="canvasStyle"
          ></canvas>
          <canvas
            id="ctx_back"
            ref="ctx_back"
            :style="canvasStyle"
          ></canvas>
          <input
            ref="text"
            name="text"
            id="text"
            class="text-input"
            @blur="handleTextBlur"
            @keyup.enter="handleTextBlur"
            v-model="text"
            autofocus
            autocomplete="off"
            :style="
              'font-size:' + (this.slide * 10 + 14) + 'px;color:' + defaultColor
            "
          />
        </div>
        </div>
        <div :class="['top_tools','tools', 'settings', isExpand ? '' : 'noExpand']">
          <div class="tool_item size_setting">
            <input id="canvas_width" v-model.number="canvasWidth" @blur="handleCanvasSizeChange" />
            <span>x</span>
            <input  id="canvas_height" v-model.number="canvasHeight" @blur="handleCanvasSizeChange" />
          </div>
          <div
            class="tool_item"
            v-for="(item, index) in settings"
            :key="item.icon"
          >
            <button v-if="index == 0">
              <svg-icon :icon-class="item.icon" :style="'color:' + defaultColor"/>
              <span :style="'color:' + defaultColor">{{ item.name }}</span>
              <el-color-picker v-model="defaultColor"></el-color-picker>
            </button>
            <div v-else-if="index == 1" class="slide">
              <!-- <svg-icon :icon-class="item.icon" /> -->
              <span style="color:#fff;">{{ item.name }}</span>
              <el-slider
                ref="slide"
                v-model="slide"
                :min="1"
                :max="10"
                :step="1"
              ></el-slider>
            </div>
            <button v-else @click.stop="item.fun">
              <svg-icon :icon-class="item.icon" />
              <span>{{ item.name }}</span>
            </button>
          </div>
          <div class="tool_item" style="width:60px;">
            <button @click.stop="handleChangeToolType(7)">选择</button>
          </div>
          <div class="tool_item" v-for="item in btns" :key="item.icon">
            <button
              @click.stop="item.fun"
              v-if="item.name == '上一步'"
              :disabled="prevDis"
              :style="prevDis ? 'cursor:not-allowed' : ''"
            >
              <svg-icon :icon-class="item.icon" />
              <span>{{ item.name }}</span>
            </button>
            <button
              @click.stop="item.fun"
              v-else-if="item.name == '下一步'"
              :disabled="nextDis"
              :style="nextDis ? 'cursor:not-allowed' : ''"
            >
              <svg-icon :icon-class="item.icon" />
              <span>{{ item.name }}</span>
            </button>
            <button @click.stop="item.fun" v-else>
              <svg-icon :icon-class="item.icon" />
              <span>{{ item.name }}</span>
            </button>
          </div>
          <div class="tool_item go_up">
            <button @click.stop="handleShowOrHide(0)">
              <i class="el-icon-caret-top"></i>
              <span>收起</span>
            </button>
          </div>
          <div class="pull" v-if="!isExpand">
            <span class="line"></span>
            <span
              class="round"
              title="展开"
              @click.stop="handleShowOrHide(1)"
            ></span>
          </div>
        </div>
        <div :class="['tools', 'bars']">
          <template v-if="deleteShow">
            <div class="right_tool" style="min-height:300px;">
              <div style="background:#f38c8c;color:#fff;padding:3px;">选择</div>
              <div
                :class="[
                  'tool_item',
                  activeTool == 'delete' ? 'activeTool' : '',
                ]"
                @click.stop="selectDelete"
              >
                <svg-icon icon-class="delete" />
                <span>删除</span>
              </div>
              <div
                :class="[
                  'tool_item',
                  activeTool == 'delete' ? 'activeTool' : '',
                ]"
                style="border-bottom: 1px solid #dddddd;"
                @click.stop="selectCancel"
              >
                <svg-icon icon-class="cancel" />
                <span style="vertical-align:1px;">取消</span>
              </div>
            </div>
          </template>
          <template v-else>
            <div
              class="el-icon-s-tools arrow"
              v-if="!showTools"
              title="展开"
              @click.stop="handleShowTools(1)"
            ></div>
            <div
              class="el-icon-arrow-right arrow"
              v-else
              title="收起"
              @click.stop="handleShowTools(0)"
            ></div>
            <transition name="slide-fade" appear>
              <div v-if="showTools" class="right_tool">
                <div style="background:#f38c8c;color:#fff;padding:3px;">工具</div>
                <div
                  :class="[
                    'tool_item',
                    activeTool == item.toolType ? 'activeTool' : '',
                  ]"
                  v-for="item in tools"
                  :key="item.toolType"
                  @click.stop="handleChangeToolType(item.toolType)"
                >
                  <svg-icon :icon-class="item.icon" />
                  <span>{{ item.name }}</span>
                </div>
              </div>
            </transition>
          </template>
        </div>
      </div>
  </div>
</template>

<script>
import cursors from './cursor'

export default {
  name: 'SimpleDrawing',
  props: {
    // 底色
    initConfig: {
      type: Object,
      default: () => {
        return {
          bgColor: '#ffffff', // 背景色 【默认白色】
          strokeColor: '#333333', // 笔触颜色 【默认黑色】
          width: 400, // 默认宽度 px 【默认400】
          height: 400 // 默认高度 px 【默认400】
        }
      }
    },
    reset: {
      type: Boolean,
      default: true
    }
  },
  computed: {
    boardStyle() {
      return {
        'width': this.currentImg.width + 'px',
        'height': this.currentImg.height + 'px'
      }
    },
    canvasStyle() {
      return {
        'cursor': this.cursor,
        'width': this.currentImg.width,
        'height': this.currentImg.height,
        'padding': '30px'
      }
    }
  },
  watch: {
    reset: {
      handler(val) {
        if (val && this.canvas_front) {
          this.handleClearCanvas()
        }
      },
      immediate: true
    }
  },
  data() {
    return {
      defaultColor: '#333333',
      bgColor: '#ffffff',
      cursor: `url('${cursors.pen}'),auto`,
      slide: 1,
      settings: [
        {
          icon: 'paint',
          name: '颜色',
          fun: ''
        },
        {
          icon: 'pen',
          name: '粗细',
          fun: ''
        }
      ],
      activeTool: 1,
      tools: [
        {
          icon: 'pen',
          name: '画笔',
          toolType: 1
        },
        {
          icon: 'line',
          name: '直线',
          toolType: 2
        },
        {
          icon: 'circle',
          name: '圆形',
          toolType: 3
        },
        {
          icon: 'square',
          name: '矩形',
          toolType: 4
        },
        {
          icon: 'eraser',
          name: '橡皮',
          toolType: 5
        },
        {
          icon: 'text',
          name: '文字',
          toolType: 6
        }
      ],
      btns: [
        {
          icon: 'pre',
          name: '上一步',
          fun: () => {
            return this.handlePrev()
          }
        },
        {
          icon: 'next',
          name: '下一步',
          fun: () => {
            return this.handleNext()
          }
        },
        {
          icon: 'delete',
          name: '清除',
          fun: () => {
            return this.handleClearCanvas()
          }
        },
        {
          icon: 'document',
          name: '保存',
          fun: () => {
            return this.handleCanvas2Img()
          }
        }
      ],
      canvas_front: null,
      canvas_back: null,
      canvas_base: null,
      ctx_base: null,
      ctx_front: null,
      ctx_back: null,
      canvasWidth: 400,
      canvasHeight: 400,
      currentImg: {
        url: undefined,
        width: 800,
        height: 800,
        scale: 1,
        index: -1
      },
      isExpand: 1,
      showTools: 1,
      canDraw: false,
      text: '',
      canvasStore: [],
      prevDis: true,
      nextDis: true,
      tl: 0,
      tt: 0,
      deleteShow: false,
      deleteX: 0,
      deleteY: 0,
      deleteWidth: 0,
      deleteHeight: 0
    }
  },
  created() {
    // TODO:未做校验
    this.canvasWidth = this.initConfig.width || 400
    this.canvasHeight = this.initConfig.height || 400
    this.defaultColor = this.initConfig.strokeColor || '#333333'
    this.bgColor = this.initConfig.bgColor || '#ffffff'
  },
  methods: {
    /** 显示或隐藏设置栏*/
    handleShowOrHide(status) {
      this.isExpand = status
    },
    /** 显示或隐藏工具栏*/
    handleShowTools(status) {
      this.showTools = status
    },
    /** 工具切换*/
    handleChangeToolType(type) {
      this.activeTool = type
      switch (type) {
        case 1:
          this.cursor = `url('${cursors.pen}'),auto`
          break
        case 2:
          this.cursor = `default`
          break
        case 3:
          this.cursor = `crosshair`
          break
        case 4:
          this.cursor = `crosshair`
          break
        case 5:
          this.cursor = `url('${cursors.eraser}'),auto`
          break
        case 6:
          this.cursor = `url('${cursors.text}'),auto`
          break
        case 7:
          this.cursor = `crosshair`
          break
        default:
          this.cursor = `url('${cursors.pen}'),auto`
          break
      }
      this.handleDrawCanvas(type)
    },
    /** 初始化画布*/
    handleInitCanvas() {
      this.currentImg = {
        url: undefined,
        width: this.canvasWidth,
        height: this.canvasHeight,
        scale: 1,
        index: -1
      }

      this.canvasStore = []
      this.prevDis = true
      this.nextDis = true

      // 用于绘制的画板
      this.canvas_front = document.getElementById('ctx_front')
      // 用于生成绘制后图片的画板
      this.canvas_back = document.getElementById('ctx_back')
      // 底图画板，橡皮擦除时获取像素放到绘制画板中，达到不擦出底图的效果
      this.canvas_base = document.getElementById('ctx_base')

      // 二维绘图
      this.ctx_base = this.canvas_base.getContext('2d')
      this.ctx_front = this.canvas_front.getContext('2d')
      this.ctx_back = this.canvas_back.getContext('2d')

      // 笔触颜色
      this.ctx_front.strokeStyle = this.defaultColor

      const width = this.currentImg.width
      const height = this.currentImg.height
      this.canvas_front.width = width
      this.canvas_front.height = height
      this.canvas_back.width = width
      this.canvas_back.height = height
      this.canvas_base.width = width
      this.canvas_base.height = height

      this.ctx_base.fillStyle = this.bgColor
      this.ctx_back.fillStyle = this.bgColor
      this.ctx_base.fillRect(0, 0, width, height)
      this.ctx_back.fillRect(0, 0, width, height)
      this.ctx_front.setLineDash([0])
      this.selectCancel()
    },
    // 改变画布大小
    handleCanvasSizeChange() {
      const oWidth = this.currentImg.width
      const oHeight = this.currentImg.height

      const width = this.canvasWidth
      const height = this.canvasHeight

      if (width < 1 || width > 2000 || height < 1 || height > 2000) {
        this.canvasWidth = oWidth
        this.canvasHeight = oHeight
        this.$message({
          message: '长宽不可小于1像素或大于2000像素',
          type: 'warning'
        })
        return
      }
      this.ctx_front.clearRect(0, 0, oWidth, oHeight)
      this.ctx_back.clearRect(0, 0, oWidth, oHeight)
      this.ctx_base.clearRect(0, 0, oWidth, oHeight)

      this.currentImg.width = width
      this.currentImg.height = height

      this.canvas_front.width = width
      this.canvas_front.height = height
      this.canvas_back.width = width
      this.canvas_back.height = height
      this.canvas_base.width = width
      this.canvas_base.height = height

      this.ctx_base.fillStyle = this.bgColor
      this.ctx_back.fillStyle = this.bgColor
      this.ctx_base.fillRect(0, 0, width, height)
      this.ctx_back.fillRect(0, 0, width, height)

      const _this = this
      const img = new Image()
      img.src = this.currentImg.url
      img.onload = function () {
        _this.ctx_front.drawImage(this, 0, 0)
        _this.ctx_back.drawImage(this, 0, 0)
      }
    },
    /** 处理放大缩小*/
    handleDrawImage() {
      const _this = this
      const img = new Image()
      img.src = this.currentImg.url
      const width = this.currentImg.width = this.currentImg.width * this.currentImg.scale
      const height = this.currentImg.height = this.currentImg.height * this.currentImg.scale
      if (this.currentImg.url) {
        img.onload = function () {
          _this.canvas_front.width = width
          _this.canvas_front.height = height
          _this.canvas_back.width = width
          _this.canvas_back.height = height
          // _this.canvas_base.width = width;
          // _this.canvas_base.height = height;
          // _this.ctx_front.drawImage(this, 0, 0);
          _this.ctx_back.drawImage(this, 0, 0)
        }
      } else {
        _this.ctx_front.clearRect(0, 0, width, height)
        _this.ctx_back.clearRect(0, 0, width, height)
        this.ctx_base.fillStyle = this.bgColor
        this.ctx_back.fillStyle = this.bgColor
        this.ctx_base.fillRect(0, 0, width, height)
        this.ctx_back.fillRect(0, 0, width, height)
      }
    },
    handleBeLarge() {
      this.currentImg.scale = 1
      this.currentImg.scale += 0.1
      this.$nextTick(() => {
        this.handleDrawImage()
      })
    },
    handleBeSmall() {
      this.currentImg.scale = 1
      this.currentImg.scale -= 0.1
      this.$nextTick(() => {
        this.handleDrawImage()
      })
    },
    /** 下载图片*/
    handleCanvas2Img() {
      // let canvas = this.canvas_back;
      const canvas = document.getElementById('ctx_back')
      this.$emit('download', canvas.toDataURL())
      this.handleClearCanvas()
      // this.$refs.download.href = canvas.toDataURL();
      // this.$refs.download.click();
      // base64
    },
    /** 清除画布*/
    handleClearCanvas() {
      this.handleInitCanvas()
    },
    handleFrommatCanvas() {
      this.ctx_front.clearRect(0, 0, this.canvas_front.width, this.canvas_front.height)
    },
    handleDrawCanvas(type) {
      this.canDraw = false
      let sx, sy, mx, my
      let offsetTop = 0
      let offsetLeft = 0
      const text = document.getElementById('text')
      // 鼠标按下
      const mousedown = (e) => {
        // 笔触颜色
        if (type === 7) {
          this.ctx_front.strokeStyle = 'red'
        } else {
          this.ctx_front.strokeStyle = this.defaultColor
        }
        // 线的粗细
        this.ctx_front.lineWidth = this.slide
        this.ctx_front.setLineDash([0])
        e = e || window.event
        const clientRect = this.canvas_front.getBoundingClientRect()
        offsetTop = clientRect.top
        offsetLeft = clientRect.left
        sx = e.clientX - offsetLeft + this.$refs.body.scrollLeft
        sy = e.clientY - offsetTop + this.$refs.body.scrollTop
        const cbx = this.ctx_base.getImageData(
          e.offsetX - this.slide / 2,
          e.offsetY - this.slide / 2,
          this.slide * 6,
          this.slide * 6
        )
        this.ctx_front.moveTo(sx, sy)
        this.canDraw = true
        switch (type) {
          case 1:
            this.ctx_front.beginPath()
            break
          case 5:
            this.ctx_front.putImageData(
              cbx,
              e.offsetX - this.slide / 2,
              e.offsetY - this.slide / 2
            )
            break
          case 6:
            this.handleTextBlur()
            this.text = ''
            text.style.fontSize = 14 + this.slide * 10 + 'px'
            text.style.color = this.defaultColor
            text.style.left =
              e.offsetX - this.$refs.body.scrollLeft + 5 + 'px'
            text.style.top =
              e.offsetY - this.$refs.body.scrollTop + 'px'
            text.style.zIndex = 10
            text.style.display = 'block'
            this.tl = e.offsetX - 25 + this.slide
            this.tt = e.offsetY - 15 + this.slide * 10
            console.log('this.tt', this.tt)
            setTimeout(() => { // 聚焦
              this.$refs.text.focus()
            }, 0)
            break
        }
      }
      const mousemove = (e) => {
        e = e || window.event
        mx = e.clientX - offsetLeft + this.$refs.body.scrollLeft
        my = e.clientY - offsetTop + this.$refs.body.scrollTop
        // mx = e.clientX
        // my = e.clientY

        const cbx = this.ctx_base.getImageData(
          e.offsetX - this.slide / 2,
          e.offsetY - this.slide / 2,
          this.slide * 6,
          this.slide * 6
        )
        if (this.canDraw) {
          switch (type) {
            case 1:
              // this.ctx_front.lineTo(mx - 53, my - 85);
              this.ctx_front.lineTo(mx - 25, my - 5)
              this.ctx_front.stroke()
              break
            case 2:
              this.handleFrommatCanvas()
              this.ctx_front.beginPath()
              this.ctx_front.moveTo(sx - 30, sy - 30 + this.slide / 2)
              this.ctx_front.lineTo(mx - 30, my - 30 + this.slide / 2)
              this.ctx_front.stroke()
              break
            case 3:
              this.handleFrommatCanvas()
              this.ctx_front.beginPath()
              // eslint-disable-next-line no-case-declarations
              const rds = Math.sqrt(
                (sx - 10 - mx) * (sx - 10 - mx) +
                  (sy - 10 - my) * (sy - 10 - my)
              )
              this.ctx_front.arc(sx - 27, sy - 27, rds, 0, Math.PI * 2, false)
              this.ctx_front.stroke()
              break
            case 4:
              this.handleFrommatCanvas()
              this.ctx_front.beginPath()
              this.ctx_front.moveTo(sx - 30, sy - 30 - this.slide / 2)
              this.ctx_front.lineTo(mx - 30 - this.slide / 2, sy - 30 - this.slide / 2)
              this.ctx_front.lineTo(mx - 30 - this.slide / 2, my - 30 - this.slide / 2)
              this.ctx_front.lineTo(sx - 30 - this.slide / 2, my - 30 - this.slide / 2)
              this.ctx_front.lineTo(sx - 30 - this.slide / 2, sy - 30 - this.slide)
              this.ctx_front.stroke()
              break
            case 5:
              this.ctx_front.putImageData(
                cbx,
                e.offsetX - this.slide / 2 - 20,
                e.offsetY - this.slide / 2 - 20
              )
              break
            case 7:
              this.handleFrommatCanvas()
              this.ctx_front.lineWidth = 2
              this.ctx_front.beginPath()
              this.ctx_front.setLineDash([5])
              this.ctx_front.moveTo(sx - 30, sy - 30 - this.ctx_front.lineWidth / 2)
              this.ctx_front.lineTo(mx - 30 - this.ctx_front.lineWidth / 2, sy - 30 - this.ctx_front.lineWidth / 2)
              this.ctx_front.lineTo(mx - 30 - this.ctx_front.lineWidth / 2, my - 30 - this.ctx_front.lineWidth / 2)
              this.ctx_front.lineTo(sx - 30 - this.ctx_front.lineWidth / 2, my - 30 - this.ctx_front.lineWidth / 2)
              this.ctx_front.lineTo(sx - 30 - this.ctx_front.lineWidth / 2, sy - 30 - this.ctx_front.lineWidth)
              this.ctx_front.stroke()
              break
          }
        }
      }
      const mouseup = () => {
        if (this.canDraw) {
          this.canDraw = false
          this.ctx_front.closePath()
          if (type === 7) {
            this.deleteX = sx - 30
            this.deleteY = sy - 30 - this.ctx_front.lineWidth / 2
            this.deleteWidth = mx - sx - this.ctx_front.lineWidth
            this.deleteHeight = my - sy
            this.deleteShow = true
          } else if (type !== 6) {
            console.log('非文字存储')
            this.handleSaveCanvasStore()
          }
        }
      }
      this.canvas_front.onmousedown = (e) => mousedown(e)
      this.canvas_front.onmousemove = (e) => mousemove(e)
      this.canvas_front.onmouseup = (e) => mouseup(e)
      this.canvas_front.onmouseout = (e) => mouseup(e)
      this.canvas_front.onmouseleave = (e) => mouseup(e)
    },
    // 删除所选区域
    selectDelete() {
      this.ctx_front.clearRect(0, 0, this.canvas_front.width, this.canvas_front.height)
      const cbx = this.ctx_base.getImageData(
        this.deleteX,
        this.deleteY,
        this.deleteWidth,
        this.deleteHeight
      )
      this.ctx_front.putImageData(
        cbx,
        this.deleteX,
        this.deleteY
      )
      this.handleSaveCanvasStore()
      this.deleteX = 0
      this.deleteY = 0
      this.deleteWidth = 0
      this.deleteHeight = 0
      this.deleteShow = false
      this.handleChangeToolType(1)
    },
    // 取消选择区域
    selectCancel() {
      this.deleteX = 0
      this.deleteY = 0
      this.deleteWidth = 0
      this.deleteHeight = 0
      this.ctx_front.clearRect(0, 0, this.canvas_front.width, this.canvas_front.height)
      this.deleteShow = false
      this.handleChangeToolType(1)
    },
    /** 失焦或者回车绘制文本，框隐藏*/
    handleTextBlur() {
      const text = document.getElementById('text')
      this.ctx_front.font = `300 ${text.style.fontSize} sans-serif`
      this.ctx_front.fillStyle = this.defaultColor
      this.ctx_front.fillText(this.text, this.tl, this.tt)
      text.style.display = 'none'
      this.text = ''
      text.value = ''
      this.handleSaveCanvasStore()
    },
    /** 上一步*/
    handlePrev() {
      if (this.currentImg.index >= 0) {
        this.nextDis = false
        this.currentImg.index -= 1
        this.prevDis = this.currentImg.index < 0
        this.currentImg.url = this.prevDis ? undefined : this.canvasStore[this.currentImg.index]
        this.currentImg.scale = 1
        this.handleDrawImage()
      } else {
        this.prevDis = true
      }
    },
    /** 下一步*/
    handleNext() {
      if (this.currentImg.index < this.canvasStore.length - 1) {
        this.prevDis = false
        this.currentImg.index += 1
        this.nextDis = this.currentImg.index === this.canvasStore.length - 1
        this.currentImg.url = this.canvasStore[this.currentImg.index]
        this.currentImg.scale = 1
        this.handleDrawImage()
      } else {
        this.nextDis = true
      }
    },
    /** 保存绘制*/
    handleSaveCanvasStore() {
      const url = this.canvas_front.toDataURL()
      const image = new Image()
      image.src = url
      image.onload = () => {
        // 清空front区域
        this.ctx_front.clearRect(
          0,
          0,
          this.canvas_front.width,
          this.canvas_front.height
        )
        this.ctx_front.drawImage(image, 0, 0, image.width, image.height)
        this.ctx_back.drawImage(image, 0, 0, image.width, image.height)
        const url2 = this.canvas_back.toDataURL()
        this.currentImg.url = url2
        this.currentImg.index += 1
        this.canvasStore.push(url2)
        this.prevDis = false
        // console.log(this.canvasStore)
      }
    }
  },
  mounted() {
    this.$nextTick(() => {
      this.handleInitCanvas()
      this.handleChangeToolType(1)
    })
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped lang="scss">
* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
  user-select: none;
}
.icon {
  width: 1em;
  height: 1em;
  vertical-align: -0.15em;
  fill: currentColor;
  overflow: hidden;
}
.imgDraw {
    // background-color: #a8a8a8;
    background-color:#ffc000;
    height:100vh;
    padding: 10px 20px;
    flex: 1;
    padding-top: 0;
    overflow: hidden;
    .text-input {
      width:400px
    }
    .d_body {
      position: relative;
      height: 100%;
      overflow-y: auto;
      padding-top: 50px;
      .board {
        position: relative;
        min-height: 100%;
        position: relative;
        min-height: 100%;
        display: flex;
        justify-content: center;
        canvas {
          // width: 100%;
          position: absolute;
          margin: 0 auto;
          left: 0;
          right: 0;
          top: 0;
        }
        #ctx_front {
          z-index: 5;
        }
        #ctx_back {
          z-index: 3;
        }
        #ctx_base {
          z-index: 1;
        }
        #text {
          position: absolute;
          z-index: -1;
          resize: none;
          outline: none;
          border: 1px dashed #eeeeee;
          overflow: hidden;
          background: transparent;
          line-height: 30px;
          display: none;
        }
      }
      .top_tools {
        align-items: center;
        .size_setting {
          >input {
            width:50px;
            height: 30px;
            padding: 5px 10px;
            background: #ffffff;
            border: 1px solid #eeeeee;
            outline: none;
          }
          span {
            margin: 0 10px;
          }
        }
      }
      .tools {
        width: 100%;
        position: absolute;
        display: flex;
        z-index: 5;
        background: #333;
        transition: all 0.2s ease-in-out;
      }
      .settings {
        top: 0;
        left: 50%;
        transform: translateX(-50%);
        z-index: 10;
        padding: 5px 10px;
        border-bottom-left-radius: 4px;
        border-bottom-right-radius: 4px;
        border: 1px solid #333;
        border-top: 0;
        width: auto;
        .tool_item {
          display: flex;
          align-items: center;
          flex-wrap: nowrap;
          &:not(:last-of-type) {
            margin-right: 25px;
          }
        }
        .go_up {
          margin-right: 0 !important;
        }
        button {
          display: flex;
          flex-wrap: nowrap;
          align-items: center;
          justify-content: center;
          padding: 5px 10px;
          background: #ffffff;
          border: 1px solid #eeeeee;
          outline: none;
          cursor: pointer;
          position: relative;
          flex-wrap: nowrap;
          svg {
            color: #333333;
            font-size: 18px;
            margin-right: 5px;
          }
          span {
            white-space: nowrap;
          }
          ::v-deep(.el-color-picker) {
            position: absolute;
            left: 0;
            top: 0;
            width: 100%;
            height: 100%;
            .el-color-picker__trigger {
              width: 100%;
              height: 100%;
              opacity: 0;
              filter: alpha(opacity=0);
            }
          }
        }
        .slide {
          width: 150px;
          display: flex;
          flex-direction: row;
          flex-wrap: nowrap;
          align-items: center;
          svg {
            font-size: 18px;
            margin-right: 5px;
          }
          ::v-deep(.el-slider) {
            flex: 1;
            width: 0;
            margin-left: 10px;
            .el-slider__button-wrapper {
              .el-slider__button {
                width: 12px;
                height: 12px;
              }
            }
          }
        }
        .pull {
          position: absolute;
          right: 20px;
          bottom: -45px;
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: flex-start;
          margin-right: 0;
          .line {
            width: 2px;
            height: 30px;
            background: dodgerblue;
          }
          .round {
            width: 15px;
            height: 15px;
            border-radius: 50%;
            background: dodgerblue;
            cursor: pointer;
          }
        }
      }
      .bars {
        position: absolute;
        top: 100px;
        right: 30px;
        .right_tool {
          position: absolute;
          top: 50px;
          right: 0;
          background-color: #ffffff;
          border-radius: 5px;
          border: 1px solid #ffc000;
        }
        .tool_item {
          cursor: pointer;
          padding: 10px;
          &:not(:last-of-type) {
            border-bottom: 1px solid #dddddd;
          }
          svg {
            font-size: 24px;
            margin-right: 8px;
          }
          span {
            font-size: 18px;
          }
          &:hover {
            svg {
              color: dodgerblue;
            }
            span {
              color: dodgerblue;
            }
          }
        }
        .activeTool {
          background: linear-gradient(to right,blue,purple);
          border-color: #fff;
          svg {
            color: #fff;
          }
          span {
            color: #fff;
          }
        }
        .arrow {
          z-index: 10;
          position: absolute;
          top: 200px;
          right: 0;
          width: 20px;
          height: 20px;
          text-align: center;
          line-height: 20px;
          border-radius: 50%;
          border: 1px solid #606266;
          color: #606266;
          position: absolute;
          left: -10px;
          background: #ffffff;
          transform: translateY(-50%);
          cursor: pointer;
        }
        .el-icon-s-tools {
          left: calc(100% - 10px);
        }
        .el-icon-arrow-right {
          left: calc(100% - 10px);
        }
      }
      .noExpand {
        top: -50px;
      }
    }
  }
  .slide-fade-enter-active {
  transition: all .3s ease;
}
.slide-fade-leave-active {
  transition: all .3s ease;
}
.slide-fade-enter, .slide-fade-leave-to{
  transform: translateX(10px);
  opacity: 0;
}
</style>
