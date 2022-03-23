<template>
  <div>
    <!--合同信息 -->
    <el-divider><span class="title">合同信息</span></el-divider>
    <el-row :gutter="40">
      <el-col v-for="item in contractList" :key="item.type" :xs="24" :sm="12" :lg="8">
        <div class="contract-item" :class="{'clickable': checkPermission(permission.detail) && item.clickable}" @click="openDetail(item)">
          <div class="contract-item-name">{{ item.type }}</div>
          <div class="contract-item-value" :style="{ color: item.color }">
            <template v-if="item.type.includes('元')">
              {{ item.value | toFixed($DP.YUAN) | toThousandFilter | emptyTextFormatter }}{{ item.rate }}
            </template>
            <template v-else>{{ item.value }}</template>
          </div>
        </div>
      </el-col>
    </el-row>
    <!-- 收入-->
    <el-divider><span class="title">收入</span></el-divider>
    <el-row :gutter="40">
      <el-col v-for="item in incomeList" :key="item.type" :xs="24" :sm="12" :lg="8">
        <div class="contract-item" :class="{'clickable': checkPermission(permission.detail) && item.clickable}" @click="openDetail(item)">
          <div class="contract-item-name">{{ item.type }}(元)</div>
          <div class="contract-item-value" :style="{ color: item.color }">{{ item.value | toFixed($DP.YUAN) | toThousandFilter | emptyTextFormatter }}</div>
        </div>
      </el-col>
    </el-row>
    <!-- 支出成本-->
    <el-divider><span class="title">管理费用</span></el-divider>
    <el-row :gutter="40">
      <el-col v-for="item in managementFeeList" :key="item.type" :xs="24" :sm="12" :lg="8">
        <div class="contract-item" :class="{'clickable': checkPermission(permission.detail) && item.clickable}" @click="openDetail(item)">
          <div class="contract-item-name">{{ item.type }}(元)</div>
          <div class="contract-item-value">{{ item.value | toFixed($DP.YUAN) | toThousandFilter | emptyTextFormatter }}</div>
        </div>
      </el-col>
    </el-row>
    <!-- 支出成本-->
    <el-divider><span class="title">生产成本</span></el-divider>
    <el-row :gutter="40">
      <el-col v-for="item in costList" :key="item.type" :xs="24" :sm="12" :lg="8">
        <div class="contract-item" :class="{'clickable': checkPermission(permission.detail) && item.clickable}" @click="openDetail(item)">
          <div class="contract-item-name">{{ item.type }}(元)</div>
          <div class="contract-item-value">{{ item.value | toFixed($DP.YUAN) | toThousandFilter | emptyTextFormatter }}</div>
        </div>
      </el-col>
    </el-row>
    <!-- 利润 -->
    <el-divider><span class="title">利润</span></el-divider>
    <el-row :gutter="40">
      <el-col v-for="item in profitList" :key="item.type" :xs="24" :sm="12" :lg="8">
        <div class="contract-item" :class="{'clickable': checkPermission(permission.detail) && item.clickable}" @click="openDetail(item)">
          <div class="contract-item-name">{{ item.type }}</div>
          <div class="contract-item-value">{{ item.value | toFixed($DP.YUAN) | toThousandFilter | emptyTextFormatter }}<span v-if="item.value && item.type === '毛利率'"> %</span></div>
        </div>
      </el-col>
    </el-row>
    <el-dialog
      v-permission="permission.detail"
      append-to-body
      :visible.sync="amountInfo.visible"
      :show-close="false"
      title="查看详情"
      top="5vh"
      width="85vw"
      @closed="amountInfo.visible = false"
    >
      <template slot="title">
        <span v-text="`${amountInfo.name}`" />
        <el-tag effect="light">{{ profitData.shortName }}</el-tag>
        <print-table
          v-permission="permission.print"
          :api-key.sync="apiKey"
          :params="params"
          size="mini"
          type="warning"
          class="filter-item"
          style="float: right"
        />
      </template>
      <template v-permission="permission.detail">
        <component
          :is="amountInfo.currentView"
          :id="amountInfo.id"
          :project-id="projectId"
          @materialStatusChange="materialStatusChange"
        />
      </template>
    </el-dialog>
  </div>
</template>
<script>

import { get } from '@/api/config-manage/base/expenseType'
import waves from '@/directive/waves'
import moment from 'moment'
import collectionAmount from './module/collection-amount'
import deliverAmount from './module/deliver-amount'
import managementFee from './module/management-fee'
import materialCost from './module/material-cost'
import productionCost from './module/production-cost'
import checkPermission from '@/utils/permission'

const permission = {
  detail: ['financial:detail'],
  print: ['financial:print']
}

export default {
  components: { collectionAmount, deliverAmount, managementFee, materialCost, productionCost },
  directives: {
    waves
  },
  props: {
    profitData: {
      type: Object,
      default: () => {}
    },
    projectId: {
      type: [String, Number],
      default: undefined
    }
  },
  data() {
    return {
      permission,
      checkPermission,
      contractList: [],
      incomeList: [],
      managementFeeList: [],
      costList: [],
      profitList: [],
      expenseType: [],
      apiKey: '',
      amountInfo: {
        id: '',
        name: '',
        visible: false,
        currentView: 'collection-amount'
      }
    }
  },
  computed: {
    params() {
      if (this.apiKey === 'PROJECT_MANAGEMENT_FEE') {
        return { projectId: this.projectId, expenseTypeId: this.amountInfo.id }
      } else if (this.apiKey === 'PROJECT_COLLECTION_TYPE_DETAIL') {
        return { projectId: this.projectId, type: this.amountInfo.id }
      }
      return this.projectId
    }
  },
  watch: {
    profitData: {
      handler() {
        this.initData()
      },
      immediate: true
    }
  },
  created() {
    this.fetchExpenseType()
  },
  methods: {
    async fetchExpenseType() {
      if (!checkPermission(this.permission.detail)) {
        return
      }
      let _expenseType = []
      try {
        const { content = [] } = await get()
        _expenseType = content
      } catch (error) {
        console.log('报销费用类型', error)
      } finally {
        this.expenseType = _expenseType
      }
    },
    openDetail(row) {
      if (checkPermission(this.permission.detail) && row.clickable) {
        this.apiKey = row.apiKey
        this.amountInfo.id = row.id
        this.amountInfo.visible = true
        this.amountInfo.name = row.type
        this.amountInfo.currentView = row.component
      }
    },
    getContractList() {
      // 合同信息
      this.contractList = []
      const {
        contractNo,
        name,
        shortName,
        contractAmount,
        signingDate,
        sendOutCompany,
        totalCollectionAmount,
        pendingPayment,
        shouldPayment,
        deliverAmount
      } = this.profitData

      const collectionRate = (totalCollectionAmount / contractAmount * 100).toFixed(2)
      const pendingPaymentRate = (pendingPayment / contractAmount * 100).toFixed(2)
      this.contractList = [{
        type: '合同编号',
        value: contractNo
      }, {
        type: '项目名称',
        value: name
      }, {
        type: '项目简称',
        value: shortName
      }, {
        type: '合同额(元)',
        value: contractAmount
      }, {
        type: '签订日期',
        value: `${signingDate ? moment(signingDate).format('YYYY-MM-DD') : '-'}`
      }, {
        type: '发包单位',
        value: sendOutCompany
      }, {
        id: 0,
        type: '收款额(元)',
        apiKey: 'PROJECT_COLLECTION_TYPE_DETAIL',
        component: 'collection-amount',
        value: totalCollectionAmount,
        clickable: true,
        rate: `（${isNaN(collectionRate) ? '-' : collectionRate}%）`
      }, {
        type: '待收款(元)',
        value: pendingPayment,
        rate: `（${isNaN(pendingPaymentRate) ? '-' : pendingPaymentRate}%）`
      }, {
        type: '发货额(元)',
        apiKey: 'PROJECT_DELIVER_AMOUNT',
        component: 'deliver-amount',
        clickable: true,
        value: deliverAmount
      }, {
        type: '应收款(元)',
        value: shouldPayment,
        color: '#0a0'
      }]
    },
    getIncomeList() {
      // 收入
      this.incomeList = []
      const {
        collectionAmount,
        exportTaxRebate
      } = this.profitData

      this.incomeList = [{
        id: 1,
        type: '收款',
        color: '#0a0',
        clickable: true,
        value: collectionAmount,
        component: 'collection-amount',
        apiKey: 'PROJECT_COLLECTION_TYPE_DETAIL'
      }, {
        id: 2,
        color: '#0a0',
        clickable: true,
        type: '出口退税',
        value: exportTaxRebate,
        component: 'collection-amount',
        apiKey: 'PROJECT_COLLECTION_TYPE_DETAIL'
      }]
      this.incomeList.push({
        type: '合计',
        value: this.sum(this.incomeList),
        color: '#0a0'
      })
    },
    getManagementFeeList() {
      // 管理费用
      this.managementFeeList = []
      const {
        otherAmount,
        managementFee
      } = this.profitData

      let _managementFeeList = [{
        type: '管理费',
        amount: managementFee
      }]
      if (otherAmount) {
        _managementFeeList = _managementFeeList.concat(otherAmount)
      }
      this.managementFeeList = _managementFeeList
      const cosSum = this.managementFeeList.reduce((prev, curr) => {
        const index = this.expenseType.findIndex(v => v.name === curr.type)
        if (~index) {
          curr.id = this.expenseType[index].id
          curr.clickable = true
          curr.component = 'management-fee'
          curr.apiKey = 'PROJECT_MANAGEMENT_FEE'
        }
        curr.value = curr.amount
        const value = Number(curr.value)
        if (!isNaN(value)) {
          return prev + curr.value * 1000
        } else {
          return prev
        }
      }, 0)
      this.managementFeeList.push({
        type: '合计',
        value: cosSum / 1000
      })
    },
    getCostList() {
      this.costList = []
      const {
        mainMaterialCost,
        auxiliaryMaterialCost,
        productionCost
      } = this.profitData

      // 生产成本
      this.costList = [{
        type: '主材成本', // FIXME: 主材成本 + 项目领取的辅材成本
        clickable: true,
        component: 'material-cost',
        apiKey: 'PROJECT_MAIN_MATERIAL_CONT',
        id: 'mainMaterial',
        value: mainMaterialCost
      }, {
        type: '辅材成本', // 辅材摊销成本
        clickable: true,
        component: 'material-cost',
        apiKey: 'PROJECT_AUXILIARY_MAIN_MATERIAL_CONT',
        id: 'auxiliaryMaterial',
        value: auxiliaryMaterialCost
      }, {
        type: '人工成本',
        clickable: true,
        component: 'production-cost',
        apiKey: 'PROJECT_PRODUCTION_CONT',
        value: productionCost
      }]
      this.costList.push({
        type: '合计',
        value: this.sum(this.costList)
      })
    },
    getProfitList() {
      // 利润
      this.profitList = []
      const {
        actualSettlementAmount,
        finalIncome,
        finalExpenditure,
        grossProfit,
        grossMargin
      } = this.profitData

      this.profitList = [{
        type: '实际决算额(元)',
        value: actualSettlementAmount
      }, {
        type: '最终收入(元)',
        value: finalIncome
      }, {
        type: '最终支出(元)',
        value: finalExpenditure
      }, {
        type: '毛利润(元)',
        value: grossProfit
      }, {
        type: '毛利率',
        value: grossMargin ? grossMargin * 100 : grossMargin
      }]
    },
    sum(list) {
      const num = list.reduce((prev, curr) => {
        const value = Number(curr.value)
        if (!isNaN(value)) {
          return prev + curr.value * 1000
        } else {
          return prev
        }
      }, 0)
      return num / 1000
    },
    initData() {
      this.getContractList()
      this.getIncomeList()
      this.getManagementFeeList()
      this.getCostList()
      this.getProfitList()
    },
    materialStatusChange(status) {
      if (status === '出库') {
        if (this.amountInfo.id === 'mainMaterial') {
          this.apiKey = 'PROJECT_MAIN_MATERIAL_CONT'
        } else {
          this.apiKey = 'PROJECT_AUXILIARY_MAIN_MATERIAL_CONT'
        }
      } else {
        if (this.amountInfo.id === 'mainMaterial') {
          this.apiKey = 'PROJECT_MAIN_MATERIAL_RETURN_CONT'
        } else {
          this.apiKey = 'PROJECT_AUXILIARY_MAIN_MATERIAL_RETURN_CONT'
        }
      }
    }
  }
}
</script>
<style scoped>
  .title {
    display: inline-block;
    width: 100px;
    text-align: center;
    padding: 5px 10px;
    background-color: rgb(0, 140, 255);
    border-radius: 15px;
    color: #fff;
    font-size: 14px;
  }
  .el-divider{
    margin: 35px 0;
  }
  .contract-item{
    display: flex;
    font-size: 14px;
    align-items: center;
  }
  .contract-item-name {
    width: 100px;
    line-height: 40px;
    color: #606266;
    font-weight: 600;
    overflow: hidden;
    white-space: nowrap;
    text-overflow: ellipsis;
  }
  .contract-item-value {
    flex: 1;
    max-height: 40px;
    line-height: 20px;
    color: #666;
    overflow : hidden;
    text-overflow: ellipsis;
    display: -webkit-box;
    -webkit-line-clamp: 2;
    -webkit-box-orient: vertical;
  }
  .clickable {
    cursor: pointer;
  }
  .clickable>.contract-item-name{
    color: #409EFF;
  }
</style>
