<template>
  <div class="app-container">
    <div>
      <el-radio-group v-model="showName" size="small" class="filter-item">
        <el-radio-button label="contract">合同信息</el-radio-button>
        <el-radio-button label="changeLog">变更管理</el-radio-button>
      </el-radio-group>
    </div>
    <transition name="el-fade-in">
      <div v-show="showName=='contract'" style="margin-bottom:10px;position:relative;">
        <div style="position:absolute;right:0;z-index:2;">
          <div style="display:flex;border:1px solid #ffe399;border-radius:4px;" class="contractbtns" v-if="!isModify">
            <template v-if="!isModify">
              <template v-if="projectStaus==0">
                <el-tooltip class="item" effect="dark" content="修改" placement="top">
                  <common-button size="mini" icon="el-icon-edit" plain class="nextbtn" @click="isModify=true;" />
                </el-tooltip>
                <el-tooltip class="item" effect="dark" content="合同金额变更" placement="top">
                  <common-button size="mini" icon="el-icon-tickets" plain class="nextbtn" @click="moneyChange" />
                </el-tooltip>
              </template>
              <el-tooltip class="item" effect="dark" content="项目结算" placement="top">
                <common-button v-if="projectStaus!=1" size="mini" icon="el-icon-money" plain class="nextbtn" @click="confirmSettle" />
              </el-tooltip>
              <el-tooltip class="item" effect="dark" content="打印" placement="top">
                <common-button size="mini" icon="el-icon-printer" plain class="nextbtn" />
              </el-tooltip>
              <el-tooltip class="item" effect="dark" content="下载" placement="top">
                <common-button size="mini" icon="el-icon-download" plain class="nextbtn" />
              </el-tooltip>
            </template>
          </div>
          <div v-else>
            <common-button size="mini" @click="isModify=false">取消</common-button>
            <common-button :loading="submitLoading" size="mini" type="success" @click="submit">保存</common-button>
          </div>
        </div>
        <el-tabs v-model="activeName" v-if="projectId">
          <el-tab-pane label="基础信息" name="baseInfo">
            <base-info  ref="baseRef" class="tab-content" :project-id="projectId" :is-modify="isModify" @changeStatus="changeEditStatus" />
          </el-tab-pane>
          <el-tab-pane label="商务信息" name="businessInfo">
            <business-info class="tab-content" :project-id="projectId" :is-modify="isModify" @changeStatus="changeEditStatus" />
          </el-tab-pane>
          <el-tab-pane label="客户信息" name="customerInfo">
            <customer-info class="tab-content" :project-id="projectId" :is-modify="isModify" @changeStatus="changeEditStatus" />
          </el-tab-pane>
          <el-tab-pane label="项目成员" name="memberInfo">
            <members v-if="projectId" ref="membersList" :project-id="projectId" :is-modify="isModify" @changeStatus="changeEditStatus" />
          </el-tab-pane>
        </el-tabs>
      </div>
    </transition>
    <!-- <transition name="el-fade-in">
      <change-log v-if="showName=='changeLog'" style="margin-top:10px;" />
    </transition> -->
    <!-- 金额变更 -->
    <money-form :audit-staus="auditStaus" :project-id="projectId" @changestaus="changeMoneyStaus" v-model="moneyVisible" :contract-info="baseInfoValue"/>
    <!-- 结算填报 -->
    <!-- <settle-form :visible.sync="settleVisible" :audit-staus="auditStaus" :project-id="projectId" @changestaus="changeSettleStaus" /> -->
  </div>
</template>

<script setup>
import { ref, defineProps, watch, computed } from 'vue'
import { ElTabs, ElTabPane } from 'element-plus'
import baseInfo from './base'
import businessInfo from './business'
import customerInfo from './customer'
import members from './members'
import moneyForm from './money-form'
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  projectStaus: {
    type: [Number, String],
    default: undefined
  },
  projectName: {
    type: String,
    default: undefined
  }
})
const showName = ref('contract')
const activeName = ref('baseInfo')
const isModify = ref(false)
const moneyVisible = ref(false)
const submitLoading = ref(false)
const auditStaus = ref()
const baseRef = ref()
const baseInfoValue = ref()
function confirmSettle(){

}

function changeMoneyStaus(){

}

function changeSettleStaus(){

}

function changeEditStatus(){
  
}
function moneyChange(){
  moneyVisible.value=true
  baseInfoValue.value = baseRef.value.detail
}

function submit(){
  isModify.value = false
}
// import baseInfo from './base'
// import businessInfo from './business'
// import customerInfo from './customer'
// import members from './members'
// import moneyForm from './money-form'
// import settleForm from './settle-form'
// import changeLog from './change-log'

// export default {
//   components: { baseInfo, businessInfo, customerInfo, members, moneyForm, settleForm, changeLog },
//   props: {
//     projectId: {
//       type: [Number, String],
//       default: undefined
//     },
//     projectStaus: {
//       type: [Number, String],
//       default: undefined
//     },
//     projectName: {
//       type: String,
//       default: undefined
//     }
//   },
//   data() {
//     return {
//       activeName: 'baseInfo',
//       showName: 'contract',
//       isModify: false,
//       moneyVisible: false,
//       settleVisible: false,
//       auditStaus: ''
//       // projectId: undefined
//     }
//   },
//   watch: {
//     projectId() {
//       this.isModify = false
//     }
//   },
//   methods: {
//     changeEditStatus(val) {
//       this.isModify = val
//     },
//     changeMoneyStaus(val) {
//       this.moneyVisible = val
//     },
//     changeSettleStaus(val) {
//       this.settleVisible = val
//     },
//     confirmSettle() {
//       if (this.projectStaus === 0) {
//         this.$confirm('"' + this.projectName + '"' + '项目正处于进行中状态，确定要办理结算?', '提示', {
//           confirmButtonText: '确定',
//           cancelButtonText: '取消',
//           type: 'warning'
//         }).then(() => {
//           this.settleVisible = true
//         }).catch(() => {
//         })
//       } else {
//         this.settleVisible = true
//       }
//     },
//     async print() {
//       this.printLoading = this.$loading({
//         target: '#printContainer',
//         lock: true,
//         text: '正在准备加入打印队列',
//         spinner: 'el-icon-loading',
//         fullscreen: false
//       })
//       try {
//         await printComponent('111')
//       } catch (error) {
//         this.$notify({ title: '加入打印队列失败，请重试', type: 'error', duration: 2500 })
//         throw new Error(error)
//       } finally {
//         this.printLoading.close()
//       }
//     }
//   }
// }
</script>

<style lang="scss" scoped>
::v-deep(el-tabs__content) {
  height: calc(100vh - 100px);
    overflow: auto;
}
::v-deep(.contractbtns .nextbtn){
  margin-left:0;
  border:0 none;
  background:#fff;
  color:#ffba00;
  border-radius:0;
  &:hover{
    background:#ffba00;
    color:#fff;
  }
}
</style>
