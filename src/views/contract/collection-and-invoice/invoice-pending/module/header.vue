<template>
  <div>
    <crudOperation :show-search="false">
      <template #optLeft>
        <div v-show="crud.props.searchToggle">
          <common-select
            :value.sync="query.invoiceType"
            :options="invoiceTypeEnum"
            all-label-text="所有发票类型"
            show-all
            type="enum"
            size="small"
            placeholder="可选择发票类型"
            style="width:200px"
            class="filter-item"
            @change="crud.toQuery"
          />
          <el-date-picker
            v-model="query.createTime"
            :default-time="['00:00:00','23:59:59']"
            type="daterange"
            range-separator=":"
            size="small"
            class="date-item filter-item"
            value-format="timestamp"
            start-placeholder="开票开始日期"
            end-placeholder="开票结束日期"
            style="width:240px"
            @change="handleDateChange"
          />
          <el-input
            v-model="query.invoiceNo"
            placeholder="可输入发票编号搜索"
            class="filter-item"
            style="width: 200px;"
            size="small"
            clearable
            @keyup.enter.native="crud.toQuery"
          />
          <el-input
            v-model="query.applicantName"
            placeholder="可输入填报人名称搜索"
            class="filter-item"
            style="width: 200px;"
            size="small"
            clearable
            @keyup.enter.native="crud.toQuery"
          />
          <rrOperation
            :crud="crud"
          />
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script>
import rrOperation from '@crud/RR.operation'
import CRUD, { header } from '@crud/crud'
import crudOperation from '@crud/CRUD.operation'
import checkPermission from '@/utils/system/check-permission'
import enumOperate, { invoiceTypeEnum } from '@/utils/enum/index'

const invoiceTypeEnumV = enumOperate.getVal(invoiceTypeEnum)

const defaultQuery = {
  invoiceType: undefined, applicantName: undefined,
  createTime: undefined, startDate: undefined, endDate: undefined,
  projectId: { value: undefined, resetAble: false },
  isProcessed: { value: false, resetAble: false }
}

export default {
  components: { crudOperation, rrOperation },
  mixins: [header(defaultQuery)],
  inject: ['permission'],
  data() {
    return {
      invoiceTypeEnum,
      invoiceTypeEnumV
    }
  },
  methods: {
    checkPermission,
    [CRUD.HOOK.handleRefresh](crud, data) {
      data.content = data.content.map(v => {
        // v.oldInvoiceNo = v.invoiceNo
        const _invoiceT = invoiceTypeEnumV[v.invoiceType]
        v.invoiceTypeAndTaxRate = _invoiceT && _invoiceT.SL
        if (this.$isNotBlank(v.taxRate)) {
          v.invoiceTypeAndTaxRate += `(${v.taxRate}%)`
        }
        return v
      })
    },
    handleDateChange() {
      if (this.query.createTime && this.query.createTime.length > 1) {
        this.query.startDate = this.query.createTime[0]
        this.query.endDate = this.query.createTime[1]
      } else {
        this.query.startDate = undefined
        this.query.endDate = undefined
      }
      this.crud.toQuery()
    }
  }
}
</script>
<style lang="scss" scoped>
// .pending-btn +button {
  // margin-left: 10px;
// }
</style>
