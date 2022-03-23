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
      <el-table-column key="applicationDate" prop="applicationDate" label="申请日期" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.applicationDate | parseTime('{y}-{m}-{d}') }}</span>
        </template>
      </el-table-column>
      <el-table-column key="reimbursementType" prop="reimbursementType" label="报销种类" align="left" min-width="100">
        <template v-slot="scope">
          <span>{{ dict.label['reimbursement_type'][scope.row.reimbursementType] | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column key="paymentAmount" prop="paymentAmount" label="实付金额(元)" align="right" min-width="130">
        <template v-slot="scope">
          <el-tag type="success" effect="plain" style="width:100%;max-width:130px">{{ scope.row.paymentAmount | toFixed($DP.YUAN) | toThousandFilter | emptyTextFormatter }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column key="payer" prop="payer" label="付款单位" min-width="120" />
      <el-table-column key="payee" prop="payee" label="收款单位" align="center" min-width="140" />
      <el-table-column key="handlerName" prop="handlerName" label="确认人" align="center" width="100px" />
    </el-table>
  </div>
</template>

<script>
import { projectReimbursementDetail } from '@/api/contract/reimbursement'
import sizeCalc from '@/mixins/sizeCalc'

const permission = {
  print: ['financial:print']
}

export default {
  mixins: [sizeCalc],
  dicts: ['reimbursement_type'],
  props: {
    id: {
      type: [String, Number],
      default: undefined
    },
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
    },
    'id': {
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
        const { content = [] } = await projectReimbursementDetail({ expenseTypeId: this.id, projectId: this.projectId })
        _list = content
      } catch (error) {
        console.log('管理费', error)
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
        if (column.property === 'paymentAmount') {
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
            sums[index] = sums[index].toFixed(this.$DP.YUAN)
          }
        }
      })
      return sums
    }
  }
}
</script>
