<template>
  <div class="head-container">
    <div v-show="crud.props.searchToggle">
      <el-select v-model="query.dateType" placeholder="收款日期" class="filter-item" style="width:100px;" size="small">
        <el-option :value="0">收款日期</el-option>
        <el-option :value="1">填报日期</el-option>
        <el-option :value="2">审核日期</el-option>
      </el-select>
      <el-date-picker
        v-model="query.createTime"
        :default-time="['00:00:00','23:59:59']"
        type="daterange"
        range-separator=":"
        size="small"
        class="date-item filter-item"
        value-format="timestamp"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width:240px"
      />
      <project-radio-button
        type="all"
        size="small"
        :value.sync="query.projectId"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        :value.sync="query.settlementStatus"
        :options="settlementStatusEnum"
        show-all
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <!--      <common-select-->
      <!--        :value.sync="query.paymentFineMode"-->
      <!--        :options="paymentFineModeEnum"-->
      <!--        all-label-text="所有付款方式"-->
      <!--        show-all-->
      <!--        type="enum"-->
      <!--        size="small"-->
      <!--        placeholder="付款方式"-->
      <!--        style="width:200px"-->
      <!--        class="filter-item"-->
      <!--        @change="crud.toQuery"-->
      <!--      />-->
      <!--      <common-select-->
      <!--        :value.sync="query.paymentReason"-->
      <!--        :options="dict.payment_reason"-->
      <!--        all-label-text="所有付款事由"-->
      <!--        show-all-->
      <!--        type="dict"-->
      <!--        size="small"-->
      <!--        placeholder="付款事由"-->
      <!--        style="width:200px"-->
      <!--        class="filter-item"-->
      <!--        @change="crud.toQuery"-->
      <!--      />-->
      <el-input
        v-model="query.applicantName"
        placeholder="填报人名称搜索"
        class="filter-item"
        style="width: 150px;"
        size="small"
        clearable
        @keyup.enter.native="crud.toQuery"
      />
      <el-input
        v-model="query.applicantName1"
        placeholder="审核人名称搜索"
        class="filter-item"
        style="width: 150px;"
        size="small"
        clearable
        @keyup.enter.native="crud.toQuery"
      />
      <rrOperation
        :crud="crud"
      />
    </div>
    <crudOperation>
      <template slot="optRight">
        <print-table
          v-permission="permission.print"
          api-key="PROJECT_INVOICE_DETAIL"
          :params="query.projectId"
          size="mini"
          type="warning"
          class="filter-item"
          style="width:280px"
        />
      </template>
      <template slot="viewLeft">
        <div v-permission="permission.get" style="display:inline-block">
          <!--          <el-tag effect="plain">-->
          <!--            合同总额：-->
          <!--            <span v-if="!totalQueryLoading">{{ contractAmount | toFixed($DP.YUAN) | toThousandFilter | emptyTextFormatter }}元</span>-->
          <!--            <i v-else class="el-icon-loading" />-->
          <!--          </el-tag>-->
          <el-tag type="success" effect="plain">
            累计开票金额：
            <span v-if="!totalQueryLoading">{{ invoiceAmount | toFixed($DP.YUAN) | toThousandFilter | emptyTextFormatter }}元</span>
            <i v-else class="el-icon-loading" />
          </el-tag>
          <!--          <el-tag type="success" effect="plain">-->
          <!--            开票率：-->
          <!--            <span v-if="!totalQueryLoading">{{ rate | toFixed(this.$DP.ACCOUNTING) | emptyTextFormatter }}%</span>-->
          <!--            <i v-else class="el-icon-loading" />-->
          <!--          </el-tag>-->
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script>
import { getExtraInfo, download } from '@/api/contract/collection/invoice'
import { editSettlementStatus } from '@/api/contract/collection/index'
import rrOperation from '@crud/RR.operation'
import CRUD, { header } from '@crud/crud'
import crudOperation from '@crud/CRUD.operation'
import checkPermission from '@/utils/permission'
import enumOperate, { settlementStatusEnum, invoiceTypeEnum } from '@/utils/enum/index'
// import popConfirmBtn from '@/views/components/common/pop-confirm-btn'

const invoiceTypeEnumV = enumOperate.getVal(invoiceTypeEnum)

const defaultQuery = {
  invoiceType: void 0, applicantName: void 0,
  confirmDate: void 0, invoiceDate: void 0, applicationDate: void 0,
  projectId: { value: void 0, resetAble: false }
}

export default {
  components: { crudOperation, rrOperation },
  mixins: [header(defaultQuery)],
  inject: ['permission'],
  data() {
    return {
      invoiceTypeEnum,
      invoiceTypeEnumV,
      settlementStatusEnum,
      settlementStatus: void 0,
      hasSettlement: void 0,
      unprocessedQuantity: 0,
      totalQueryLoading: false,
      pendingVisible: false,
      invoiceAmount: '查询中...',
      contractAmount: '查询中...',
      rate: '查询中...'
    }
  },
  mounted() {
    this.$BUS.$on('handleSettlementStatusChange', (hasSettlement) => {
      this.hasSettlement = hasSettlement
      this.settlementStatus = hasSettlement ? settlementStatusEnum.SETTLED.V : settlementStatusEnum.UNSETTLEMENT.V
      this.crud.optShow.add = !hasSettlement
      // this.crud.optShow.del = !hasSettlement
    })
  },
  beforeDestroy() {
    this.$BUS.$off('handleSettlementStatusChange')
  },
  methods: {
    download,
    checkPermission,
    editSettlementStatus,
    openPendingList() {
      this.pendingVisible = true
    },
    handleSettlementStatusChange() {
      // 取反
      this.settlementStatus = this.hasSettlement ? settlementStatusEnum.UNSETTLEMENT.V : settlementStatusEnum.SETTLED.V
      this.hasSettlement = !this.hasSettlement
      this.crud.optShow.add = !this.hasSettlement
      // this.crud.optShow.del = !this.hasSettlement
      this.$BUS.$emit('handleSettlementStatusChange', this.hasSettlement)
    },
    async [CRUD.HOOK.beforeToQuery]() {
      const applicationDate = this.query.applicationDate
      if (applicationDate && applicationDate.length > 1) {
        this.query.applicationStartDate = applicationDate[0]
        this.query.applicationEndDate = applicationDate[1]
      } else {
        this.query.applicationStartDate = void 0
        this.query.applicationEndDate = void 0
      }
      const invoiceDate = this.query.invoiceDate
      if (invoiceDate && invoiceDate.length > 1) {
        this.query.invoiceStartDate = invoiceDate[0]
        this.query.invoiceEndDate = invoiceDate[1]
      } else {
        this.query.invoiceStartDate = void 0
        this.query.invoiceEndDate = void 0
      }
      const confirmDate = this.query.confirmDate
      if (confirmDate && confirmDate.length > 1) {
        this.query.confirmStartDate = confirmDate[0]
        this.query.confirmEndDate = confirmDate[1]
      } else {
        this.query.confirmStartDate = void 0
        this.query.confirmEndDate = void 0
      }
      this.fetchExtraInfo()
    },
    async [CRUD.HOOK.afterDelete]() {
      this.fetchExtraInfo()
    },
    async [CRUD.HOOK.afterSubmit]() {
      this.fetchExtraInfo()
    },
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
    async fetchExtraInfo() {
      if (!checkPermission(this.permission.get) || !this.query.projectId) {
        return
      }
      try {
        this.totalQueryLoading = true
        const { invoiceAmount, contractAmount, customerName, signingMainBodyName, settlementStatus, unprocessedQuantity } = await getExtraInfo(this.query.projectId) || {}
        this.invoiceAmount = invoiceAmount
        this.contractAmount = contractAmount
        this.settlementStatus = settlementStatus
        this.hasSettlement = this.settlementStatus === settlementStatusEnum.SETTLED.V
        this.unprocessedQuantity = unprocessedQuantity
        this.crud.optShow.add = !this.hasSettlement
        // this.crud.optShow.del = !this.hasSettlement
        this.rate = invoiceAmount && contractAmount ? ((invoiceAmount / contractAmount) * 100) : contractAmount ? 0 : void 0
        this.$BUS.$emit('invoiceExtraInfo', { customerName, signingMainBodyName })
        this.$BUS.$emit('unprocessedInvoiceQuantity', this.unprocessedQuantity)
      } catch (error) {
        console.log(error)
        this.invoiceAmount = '查询失败'
        this.contractAmount = '查询失败'
        this.rate = '查询失败'
      } finally {
        this.totalQueryLoading = false
      }
    }
  }
}
</script>
<style lang="scss" scoped>
// .pending-btn +button {
  // margin-left: 10px;
// }
</style>
