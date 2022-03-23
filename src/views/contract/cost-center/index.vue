<template>
  <!-- <div id="main" v-permission="permission.get"> -->
  <div id="main">
    <!-- 卡片 -->
    <el-row :gutter="20" class="panel-group">
      <el-col :span="4" class="card-panel-col">
        <panel :name="`合同金额（万元）`" num-color="#1890ff" :decimals="2" :end-val="(1000 * 1) / 10000 || 0" />
      </el-col>
      <el-col :span="4" class="card-panel-col">
        <panel :name="`结算额（万元）`" num-color="#1890ff" :decimals="2" :end-val="(1000 * 1) / 10000 || 0" />
      </el-col>
      <el-col :span="4" class="card-panel-col">
        <panel :name="`累计成本（万元）`" num-color="#1890ff" :decimals="2" :end-val="(1000 * 1) / 10000 || 0" />
      </el-col>
      <el-col :span="4" class="card-panel-col">
        <panel :name="`毛利（万元）`" num-color="#1890ff" :decimals="2" :end-val="(1000 * 1) / 10000 || 0" />
      </el-col>
      <el-col :span="4" class="card-panel-col">
        <panel :name="`毛利率(%)`" num-color="#1890ff" :decimals="2" :end-val="100 * 1 || 0" />
      </el-col>
    </el-row>
    <el-divider><span class="title">项目概况</span></el-divider>
    <el-descriptions class="margin-top" :column="3" border>
      <el-descriptions-item label-class-name="contractLabel" label="合同编号">CMJKGS001</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="项目名称">湖北合信智能环保包装工业4.0智能工厂项目</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="合同额">1,000,000</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="签订日期">2022/1/1</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="业务负责人">顾炎煜</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="项目类型">加工订单</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="发包单位">湖北合信智能包装科技有限公司</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="累计收款">8,000,000</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="累计开票">8,000,000</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="销项税额">10,000</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="进项税额">9,000</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="预估应交税额">1,000</el-descriptions-item>
    </el-descriptions>
    <el-divider><span class="title">实时成本</span></el-divider>
     <el-descriptions class="margin-top" :column="3" border>
      <el-descriptions-item label-class-name="contractLabel" label="主材成本">1,000</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="辅材成本">1,000</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="人工成本">1,000</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="分包成本">1,000</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="物流费">1,000</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="项管费">-</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="管理费">1,000</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="财务费">-</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="厂房折旧">1,000</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="设备折旧">1,000</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="管理人员摊销">-</el-descriptions-item>
    </el-descriptions>
    <!-- <el-row :gutter="70">
      <el-col :xs="12" :sm="12" :lg="6">
        <panel :name="`合同金额（万元）`" num-color="#1890ff" :decimals="2" :end-val="(contractAmount * 1) / 10000 || 0" />
      </el-col>
      <el-col :xs="12" :sm="12" :lg="6">
        <panel :name="`成本（万元）`" num-color="#1890ff" :decimals="2" :end-val="(totalAmount * 1) / 10000 || 0" />
      </el-col>
      <el-col :xs="12" :sm="12" :lg="6">
        <panel :name="`利润（万元）`" num-color="#1890ff" :decimals="2" :end-val="(profit * 1) / 10000 || 0" />
      </el-col>
      <el-col :xs="12" :sm="12" :lg="6">
        <panel :name="`利润率(%)`" num-color="#1890ff" :decimals="2" :end-val="profitCost * 1 || 0" />
      </el-col>
    </el-row> -->
    <!-- <el-card class="box-card" style="padding: 10px">
      <div style="text-align: right">
        <el-tag v-if="checkPermission(permission.detail)" style="float: left" effect="plain">点击蓝色字体可查看详情</el-tag>
        <export-button
          v-permission="permission.export"
          :params="{projectId: globalProjectId}"
          :fn="downloadMete"
          show-btn-text
          btn-text="下载项目财报"
        />
      </div>
      <showProfit ref="profit" :profit-data="profitData" :project-id="globalProjectId" :permission="permission" />
    </el-card> -->
  </div>
</template>
<script setup>
// import waves from '@/directive/waves' // waves directive
// import showProfit from './show-profit/index'
import Panel from '@/components/Panel'
// import checkPermission from '@/utils/permission'
// import {
//   fetchProfitData,
//   fetchProfitDownLoad as downloadMete
// } from '@/api/contract/costCenter/financial-report'
// import acceptDetail from './charts/acceptDetail'
// import { mapGetters } from 'vuex'

// const permission = {
//   get: ['financial:get'],
//   detail: ['financial:detail'],
//   export: ['financial:export']
// }

// export default {
//   name: 'FinancialReport',
//   components: {
//     showProfit,
//     Panel
//   },
//   directives: {
//     waves
//   },
//   data() {
//     return {
//       myFixed: 2,
//       permission,
//       checkPermission,
//       totalAmount: 0,
//       payMsg: [],
//       profitData: {},
//       contractAmount: '0',
//       profit: '0',
//       profitCost: '0'
//     }
//   },
//   computed: {
//     ...mapGetters(['globalProjectId'])
//   },
//   watch: {
//     globalProjectId: {
//       handler() {
//         this.initData(this.globalProjectId)
//       },
//       immediate: true
//     }
//   },
//   methods: {
//     // 财报下载
//     downloadMete,
//     // 计算成本利润率
//     GetPercent(num, total) {
//       num = parseFloat(num)
//       total = parseFloat(total)
//       if (isNaN(num) || isNaN(total)) {
//         return '-'
//       }
//       return total <= 0 ? '0' : (Math.round(num / total * 10000) / 100.00)
//     },
//     async initData(id) {
//       if (!checkPermission(permission.get)) {
//         return
//       }
//       const loading = this.$loading({
//         target: '#main',
//         lock: true,
//         text: '正在加载',
//         background: 'rgba(255, 255, 255, 0.75)',
//         fullscreen: false
//       })
//       let _profitData = {}
//       try {
//         const params = {
//           projectId: id
//         }
//         _profitData = await fetchProfitData(params) || {}
//       } catch (error) {
//         console.log('获取项目财报', error)
//       } finally {
//         // 财报信息
//         this.profitData = _profitData

//         // 合同金额
//         this.contractAmount = this.profitData.contractAmount ? this.profitData.contractAmount.toFixed(2) : 0
//         // 利润额
//         this.profit = (this.profitData.contractAmount || 0) - (this.calcCostTotal() || 0)
//         // 利润率
//         this.profitCost = this.GetPercent(this.profit, this.profitData.contractAmount)
//         loading.close()
//       }
//     },
//     calcCostTotal() {
//       const {
//         mainMaterialCost,
//         auxiliaryMaterialCost,
//         productionCost,
//         managementFee,
//         otherAmount
//       } = this.profitData
//       const list = [
//         { 'type': '主材成本', 'amount': mainMaterialCost },
//         { 'type': '辅材成本', 'amount': auxiliaryMaterialCost },
//         { 'type': '生产人工成本', 'amount': productionCost },
//         { 'type': '管理费', 'amount': managementFee }
//       ]
//       if (otherAmount) {
//         this.payMsg = list.concat(otherAmount)
//       }
//       // 计算成本
//       this.totalAmount = 0
//       for (var i = 0; i < this.payMsg.length; i++) {
//         this.totalAmount += this.payMsg[i].amount
//       }
//       return this.totalAmount
//     }
//   }
// }
</script>
<style lang="scss" scoped>
#main {
  padding: 0 20px 20px;
  .box-card{
    margin-top: 20px;
  }
  .el-col{
    margin-top: 20px;
  }
}
</style>
