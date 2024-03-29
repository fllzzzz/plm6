<template>
  <div :id="id" :style="{width}" />
</template>

<script setup>
import { ref, computed, watch, defineProps, inject, onMounted, onBeforeUnmount } from 'vue'

import * as echarts from 'echarts'

const props = defineProps({
  id: {
    type: String,
    required: true
  },
  width: {
    type: String,
    default: '100%'
  },
  height: {
    type: String,
    default: '400px'
  }
})

const projectInfo = inject('projectInfo')

const chart = ref()
const provinceList = ref([])

const provinceNameList = computed(() => {
  return provinceList.value.map(v => v.provinceName)
})

const quantityList = computed(() => {
  return provinceList.value.map(v => v.quantity)
})

const fontWidth = computed(() => {
  let num = Math.max(...provinceNameList.value.map(v => v.length), 2)
  if (num > 5) {
    num = 5
  }
  return num * 16 + 10
})

const chartHeight = computed(() => {
  return (provinceList.value.length || 4) * 60
})

watch(
  () => projectInfo,
  (val) => {
    provinceList.value = val.provinceList || []
    updateChart()
  },
  { deep: true }
)

// 初始化
onMounted(() => {
  initChart()
})

// 销毁
onBeforeUnmount(() => {
  beforeDestroy()
})

function beforeDestroy() {
  if (chart.value) {
    chart.value.dispose()
    chart.value = null
  }
}

// 更新
function updateChart() {
  try {
    if (chart.value) {
      chart.value.setOption({
        grid: setGrid(),
        yAxis: setYAxis(),
        series: setSeries()
      })
    } else {
      initChart()
    }
    // 改变高度
    chart.value.resize({ height: chartHeight.value })
  } catch (error) {
    console.log('更新chart', error)
  }
}

function setYAxis() {
  return [
    {
      type: 'category',
      data: [...provinceNameList.value],
      axisLine: { show: false }, // 坐标轴
      axisTick: [{ // 坐标轴小标记
        show: false
      }],
      axisLabel: {
        textStyle: {
          fontSize: '14'
        }
      }
    }
  ]
}

function setSeries() {
  return [
    {
      name: '',
      type: 'bar',
      tooltip: { show: false },
      barMinHeight: 30, // 最小柱高
      barWidth: 40, // 柱宽度
      barMaxWidth: 100, // 最大柱宽度
      data: [...quantityList.value],
      itemStyle: {
        normal: { // 柱状图颜色
          color: '#1890ff',
          label: {
            show: true, // 显示文本
            position: 'inside', // 数据值位置
            textStyle: {
              fontSize: '14'
            }
          }
        }
      }
    }
  ]
}

function setGrid() {
  return [
    { // 绘图区调整
      x: fontWidth.value, // 左留白
      y: 0, // 上留白
      x2: 10, // 右留白
      y2: 0 // 下留白
    }
  ]
}

// 初始化
function initChart() {
  chart.value = echarts.init(document.getElementById(props.id))
  chart.value.clear()
  chart.value.setOption({
    tooltip: {
      trigger: 'item' // 悬浮提示框不显示
    },
    grid: setGrid(),
    xAxis: [
      {
        show: false,
        type: 'value',
        boundaryGap: [0, 0],
        position: 'top'
      }
    ],
    yAxis: setYAxis(),
    series: setSeries()
  })
}
</script>
