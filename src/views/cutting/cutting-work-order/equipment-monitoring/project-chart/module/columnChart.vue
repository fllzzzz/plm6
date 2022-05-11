<template>
  <div id="myEcharts" style="width:550px;height:230px;" />
</template>

<script setup>
// import { ref, computed, watch, defineProps, inject, onMounted, onBeforeUnmount } from 'vue'
import { ref, onMounted, onBeforeUnmount } from 'vue'

import * as echarts from 'echarts'

// const props = defineProps({
//   id: {
//     type: String,
//     required: true
//   },
//   width: {
//     type: String,
//     default: '600px'
//   },
//   height: {
//     type: String,
//     default: '400px'
//   }
// })

// const projectInfo = inject('projectInfo')

const chart = ref()
// const provinceList = ref([])

// const provinceNameList = computed(() => {
//   return provinceList.value.map(v => v.provinceName)
// })

// const quantityList = computed(() => {
//   return provinceList.value.map(v => v.quantity)
// })

// const fontWidth = computed(() => {
//   let num = Math.max(...provinceNameList.value.map(v => v.length), 2)
//   if (num > 5) {
//     num = 5
//   }
//   return num * 16 + 10
// })

// const chartHeight = computed(() => {
//   return (provinceList.value.length || 4) * 60
// })

// watch(
//   () => projectInfo,
//   (val) => {
//     provinceList.value = val.provinceList || []
//     updateChart()
//   },
//   { deep: true }
// )

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
// function updateChart() {
//   try {
//     if (chart.value) {
//       chart.value.setOption({
//         grid: setGrid(),
//         yAxis: setYAxis(),
//         series: setSeries()
//       })
//     } else {
//       initChart()
//     }
//     // 改变高度
//     chart.value.resize({ height: chartHeight.value })
//   } catch (error) {
//     console.log('更新chart', error)
//   }
// }

function setXAxis() {
  return [
    {
      type: 'category',
      // data: [...provinceNameList.value],
       data: ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'],
      // axisLine: { show: false }, // 坐标轴
      // axisTick: [{ // 坐标轴小标记
      //   show: false
      // }],
      // axisLabel: {
      //   textStyle: {
      //     fontSize: '14'
      //   }
      // }
    }
  ]
}

function setYAxis() {
  return [
    {
      show: true,
      type: 'value',
    }
  ]
}

function setSeries() {
  return [
    {
    //   name: '',
    //   type: 'bar',
    //   tooltip: { show: false },
      barMinHeight: 30, // 最小柱高
      barWidth: 40, // 柱宽度
      barMaxWidth: 100, // 最大柱宽度
    //   // data: [...quantityList.value],
    //   data: [120, 200, 150, 80, 70, 110, 130],
    //   itemStyle: {
    //     normal: { // 柱状图颜色
    //       color: '#1890ff',
    //       label: {
    //         show: true, // 显示文本
    //         position: 'inside', // 数据值位置
    //         textStyle: {
    //           fontSize: '14'
    //         }
    //       }
    //     }
    //   }
    data: [120, 100, 90, 80, 70, 110, 130],
      type: 'bar',
      showBackground: true,
      backgroundStyle: {
        color: 'rgba(180, 180, 180, 0.2)'
      }
    }
  ]
}

function setGrid() {
  return [
    { // 绘图区调整
      x: 0, // 左留白
      y: 8, // 上留白
      x2: 20, // 右留白
      y2: 0, // 下留白
      containLabel: true,
    }
  ]
}

// 初始化
function initChart() {
  chart.value = echarts.init(document.getElementById("myEcharts"))
  chart.value.clear()
  chart.value.setOption({
    tooltip: {
      trigger: 'item' // 悬浮提示框不显示
    },
    grid: setGrid(),
    yAxis: setYAxis(),
    xAxis: setXAxis(),
    series: setSeries()
  })
}
</script>
<style lang="scss" scoped>
    #myEcharts {
        margin-top: 20px;
    }
</style>
