<template>
  <div>
    <el-table
      v-loading="loading"
      :border="$TBS.BORDER"
      :stripe="$TBS.STRIPE"
      :data="list"
      :max-height="$_height"
      show-summary
      :summary-method="getSummaries"
      empty-text="暂无数据"
      style="width: 100%;"
    >
      <el-table-column v-if="$TBS.INDEX" :label="$TBS.INDEX_LABEL ? '序号': ''" type="index" align="center" width="60" />
      <el-table-column prop="name" label="名称" align="center" min-width="120" />
      <el-table-column prop="serialNumber" label="编号" align="center" min-width="120" />
      <el-table-column prop="completeQuantity" label="数量" align="center" />
      <el-table-column prop="weight" label="单量" align="center" min-width="120" />
      <el-table-column prop="completeWeight" label="总量" align="center" min-width="120" />
      <el-table-column key="unitPrice" prop="unitPrice" label="单价（元）" align="center">
        <template v-slot="scope">
          {{ scope.row.unitPrice | toFixed($DP.YUAN) | emptyTextFormatter }}
        </template>
      </el-table-column>
      <el-table-column key="wage" prop="wage" label="总额（元）" align="center">
        <template v-slot="scope">
          {{ scope.row.wage | toFixed($DP.YUAN)| emptyTextFormatter }}
        </template>
      </el-table-column>
    </el-table>
  </div>
</template>

<script>
import { productionCost } from '@/api/contract/costCenter/financial-report'
import sizeCalc from '@/mixins/sizeCalc'

const permission = {
  print: ['financial:print']
}

export default {
  mixins: [sizeCalc],
  props: {
    projectId: {
      type: [String, Number],
      default: undefined
    }
  },
  data() {
    return {
      extraHeight: 250,
      loading: false,
      permission,
      list: []
    }
  },
  watch: {
    'projectId': {
      handler(val) {
        this.fetchDetail()
      },
      immediate: true
    }
  },
  methods: {
    async fetchDetail() {
      let _list = []
      this.loading = true
      try {
        const { content = [] } = await productionCost(this.projectId) || []
        _list = content
      } catch (error) {
        console.log('人工成本', error)
      } finally {
        this.loading = false
        this.list = _list
      }
    },
    getSummaries(param) {
      const { columns, data } = param
      const sums = []
      columns.forEach((column, index) => {
        if (index === 0) {
          sums[index] = '合计'
          return
        }
        if (column.property === 'completeQuantity' || column.property === 'wage' || column.property === 'completeWeight') {
          const values = data.map(item => Number(item[column.property]))
          if (!values.every(value => isNaN(value))) {
            sums[index] = values.reduce((prev, curr) => {
              const value = Number(curr)
              if (!isNaN(value)) {
                return prev + curr
              } else {
                return prev
              }
            }, 0)
            if (column.property === 'completeWeight' || column.property === 'wage') {
              sums[index] = sums[index].toFixed(column.property === 'wage' ? this.$DP.YUAN : 2)
            }
          }
        }
      })
      return sums
    }
  }
}
</script>
