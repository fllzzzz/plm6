import * as echarts from 'echarts'
import { onMounted, ref } from 'vue'
import { getQhseList } from '@/api/mes/production-manage/dashboard/project-dashboard'
import checkPermission from '@/utils/system/check-permission'

export default function useQhseRecordEcharts({ elementId, title, xAxisData, globalProjectId, monomerId, permission }) {
  let myChart = null
  const echartsLoading = ref(false)
  const list = ref([])

  async function fetchList() {
    if (!checkPermission(permission.shipGet)) {
      return
    }
    if (!monomerId) {
      return
    }
    try {
      const content = await getQhseList({
        projectId: globalProjectId.value,
        monomerId: monomerId.value
      })
      list.value = content
    } catch (error) {
      console.log('qhse信息', error)
    }
  }

  onMounted(async () => {
    initChart(elementId, [])
    await updateChart()
  })

  async function updateChart() {
    try {
      echartsLoading.value = true
      await fetchList()
    } catch (error) {
      console.log('qhse信息', error)
    }
    var option = myChart.getOption()
    const yData = list.value.map(v => {
      return {
        value: v.quantity
      }
    })
    const xData = list.value.map(v => v.qualityType)
    const total = list.value.reduce((res, cur) => {
      res += cur.quantity || 0
      return res
    }, 0)
    option.series[0].data = yData
    option.xAxis[0].data = xData
    option.title[0].text = total ? 'QC事件：' + total : 'QC事件'
    myChart.setOption(option)
    echartsLoading.value = false
  }

  function initChart(elementId, xAxisData) {
    var chartDom = document.getElementById(elementId)
    myChart = echarts.init(chartDom, null, { locale: 'ZH' })
    var option

    option = {
      title: {
        text: 'QC事件',
        left: 'center'
      },
      grid: {
        left: '4.5%',
        right: '1%',
        bottom: '8%'
      },
      tooltip: {
        trigger: 'axis'
      },
      color: ['#48cfae'],
      toolbox: {
        right: 20,
        show: true,
        feature: {
          magicType: {
            show: true,
            type: ['line', 'bar']
          },
          saveAsImage: {
            show: true
          }
        }
      },
      xAxis: {
        type: 'category',
        data: xAxisData
      },
      yAxis: {
        type: 'value'
      },
      series: [{
        data: [],
        type: 'bar'
      }]
    }

    option && myChart.setOption(option)
  }

  return {
    updateChart,
    echartsLoading
  }
}
