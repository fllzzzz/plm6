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
      <el-table-column key="paymentDate" prop="paymentDate" sortable="custom" label="收款日期" align="center" min-width="140">
        <template v-slot="scope">
          <span>{{ scope.row.paymentDate | parseTime('{y}-{m}-{d}') }}</span>
        </template>
      </el-table-column>
      <el-table-column key="paymentAmount" prop="paymentAmount" sortable="custom" label="收款金额(元)" align="right" min-width="130">
        <template v-slot="scope">
          <el-tag type="success" effect="plain" style="width:100%;max-width:130px">{{ scope.row.paymentAmount | toFixed($DP.YUAN) | toThousandFilter | emptyTextFormatter }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column key="payer" prop="payer" sortable="custom" label="付款方" min-width="120" />
      <el-table-column key="paymentReason" prop="paymentReason" label="付款事由" align="center" min-width="100">
        <template v-slot="scope">
          <span>{{ dict.label['payment_reason'][scope.row.paymentReason] | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column key="collectionCompany" sortable="custom" prop="collectionCompany" label="收款方" align="center" min-width="140" />
      <el-table-column key="applicantName" prop="applicantName" label="填报人" align="center" min-width="100px" />
    </el-table>
  </div>
</template>

<script>
import { getCollectionDetail as get } from '@/api/contract/costCenter/financial-report'
import enumOperate, { paymentFineModeEnum } from '@/utils/enum/index'
import sizeCalc from '@/mixins/sizeCalc'

const paymentFineModeEnumV = enumOperate.getVal(paymentFineModeEnum)

const permission = {
  print: ['financial:print']
}

export default {
  mixins: [sizeCalc],
  dicts: ['reimbursement_type', 'payment_reason'],
  props: {
    projectId: {
      type: [String, Number],
      default: undefined
    },
    id: {
      type: [Number],
      default: 0
    }
  },
  data() {
    return {
      extraHeight: 250,
      loading: false,
      permission,
      paymentFineModeEnum,
      paymentFineModeEnumV,
      list: []
    }
  },
  watch: {
    'projectId': {
      handler() {
        this.fetchDetail()
      }
    },
    'id': {
      handler() {
        this.fetchDetail()
      }
    }
  },
  created() {
    this.fetchDetail()
  },
  methods: {
    async fetchDetail() {
      let _list = []
      this.loading = true
      try {
        const { content = [] } = await get({ projectId: this.projectId, type: this.id })
        _list = content
      } catch (error) {
        console.log('收款额', error)
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
