<template>
  <div class="scale-container">
    <div class="icon-box" @click="scaleZoom">
      <svg-icon class="icon" icon-class="zoom" />
    </div>
    <div class="icon-box" @click="scaleZoomOut">
      <svg-icon class="icon" icon-class="zoom-out" />
    </div>
    <div class="icon-box" @click="reset">
      <svg-icon class="icon" icon-class="restore-size" />
    </div>
  </div>
</template>

<script>
import { throttle } from '@/utils'
let intervals = 200
export default {
  props: {
    value: {
      type: Number,
      default: 1
    },
    step: {
      type: Number,
      default: 0.1
    },
    min: {
      type: Number,
      default: 0.5
    },
    max: {
      type: Number,
      default: 2
    },
    intervals: {
      type: Number,
      default: intervals
    }
  },
  watch: {
    intervals(val) {
      intervals = val
    }
  },
  created() {
    intervals = this.intervals
  },
  methods: {
    scaleZoom: throttle(function () {
      let val = this.value + this.step
      val = val > this.max ? this.max : val
      if (val !== this.value) {
        this.$emit('update:value', +val.toFixed(1))
        this.$emit('zoom', +val.toFixed(1))
      }
    }, intervals),
    scaleZoomOut: throttle(function () {
      let val = this.value - this.step
      val = val < this.min ? this.min : val
      if (val !== this.value) {
        this.$emit('update:value', +val.toFixed(1))
        this.$emit('zoom-out', +val.toFixed(1))
      }
    }, intervals),
    reset: throttle(function () {
      if (this.value !== 1) {
        this.$emit('update:value', 1)
        this.$emit('reset')
      }
    }, intervals)
  }
}
</script>

<style lang="scss" scoped>
.scale-container {
    user-select: none;
}
.icon-box {
    position: relative;
    display: inline-flex;
    justify-content: center;
    align-items: center;
    width: 25px;
    height: 25px;
    cursor: pointer;
    background: rgb(83, 83, 82);
    border-radius: 50%;
    opacity: 0.5;
    :hover{
        width: 80%;
        height: 80%;
    }
    &:hover{
        opacity: 1;
    }
}
.icon {
    position: absolute;
    width: 15px;
    height: 15px;
}
</style>

