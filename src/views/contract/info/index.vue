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
              <template v-if="projectstatus==0">
                <el-tooltip class="item" effect="dark" content="修改" placement="top">
                  <common-button size="mini" icon="el-icon-edit" plain class="nextbtn" @click="isModify=true;" />
                </el-tooltip>
                <el-tooltip class="item" effect="dark" content="合同金额变更" placement="top">
                  <common-button size="mini" icon="el-icon-tickets" plain class="nextbtn" @click="moneyChange" />
                </el-tooltip>
              </template>
              <el-tooltip class="item" effect="dark" content="项目结算" placement="top">
                <common-button v-if="projectstatus!=1" size="mini" icon="el-icon-money" plain class="nextbtn" @click="confirmSettle" />
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
            <base-info  ref="baseRef" class="tab-content" :project-id="projectId" :is-modify="isModify" />
          </el-tab-pane>
          <el-tab-pane label="商务信息" name="businessInfo">
            <business-info ref="businessRef" class="tab-content" :project-id="projectId" :is-modify="isModify" />
          </el-tab-pane>
          <el-tab-pane label="客户信息" name="customerInfo">
            <customer-info ref="customerRef" class="tab-content" :project-id="projectId" :is-modify="isModify" />
          </el-tab-pane>
          <el-tab-pane label="项目成员" name="memberInfo">
            <members ref="memberRef" v-if="projectId" :project-id="projectId" :is-modify="isModify" />
          </el-tab-pane>
        </el-tabs>
      </div>
    </transition>
    <transition name="el-fade-in">
      <change-audit-log v-if="showName=='changeLog'" style="margin-top:10px;" :project-id="projectId" />
    </transition>
    <!-- 金额变更 -->
    <money-form ref="moneyRef" :audit-status="auditStatus" :project-id="projectId" v-model="moneyVisible" :contract-info="baseInfoValue"/>
    <!-- 结算填报 -->
    <settle-form ref="settleRef" :audit-status="auditStatus" :project-id="projectId" v-model="settleVisible" :contract-info="baseInfoValue"/>
  </div>
</template>

<script setup>
import { ref, defineProps, watch } from 'vue'
import { ElMessage, ElTabs, ElTabPane, ElRadioGroup, ElMessageBox, ElNotification } from 'element-plus'
import { contractChangeTypeEnum } from '@enum-ms/contract'
import baseInfo from './base'
import businessInfo from './business'
import customerInfo from './customer'
import members from './members'
import moneyForm from './money-form'
import settleForm from './settle-form'
import changeAuditLog from './change-audit-log'
import { editContract } from '@/api/contract/project'

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  projectStatus: {
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
const settleVisible = ref(false)
const submitLoading = ref(false)
const auditStatus = ref()
const baseRef = ref()
const businessRef = ref()
const customerRef = ref()
const memberRef = ref()
const baseInfoValue = ref()
const moneyRef = ref()
const settleRef = ref()

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      showName.value = 'contract'
    }
  },
  { deep: true, immediate: true }
)

function confirmSettle() {
  baseInfoValue.value = baseRef.value.detail
  if (props.projectStatus === 0) {
    ElMessageBox.confirm('"' + props.projectName + '"' + '项目正处于进行中状态，确定要办理结算?', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    }).then(() => {
      settleVisible.value = true
    }).catch(() => {
    })
  } else {
    settleVisible.value = true
  }
}

function moneyChange() {
  moneyVisible.value = true
  baseInfoValue.value = baseRef.value.detail
}

const validateData = [
  { name: 'baseInfo', label: '基础信息' },
  { name: 'businessInfo', label: '商务信息' },
  { name: 'customerInfo', label: '客户信息' }
]

// function nextStepValidate(index) {
//   switch (index) {
//     case 0:
//       return baseRef.value.validateForm()
//     case 1:
//       return businessRef.value.validateForm()
//     case 2:
//       return customerRef.value.validateForm()
//   }
// }
async function submit() {
  const baseValidate = await baseRef.value.validateForm()
  const businessValidate = await businessRef.value.validateForm()
  const customerValidate = await customerRef.value.validateForm()
  const validateArr = [baseValidate, businessValidate, customerValidate]
  for (let i = 0; i < validateArr.length; i++) {
    if (!validateArr[i]) {
      activeName.value = validateData[i].name
      ElMessage.error(validateData[i].label + '请完善!')
      return
    }
  }
  memberRef.value.getUser()
  // const userData = memberRef.value.getUser()
  const submitform = {
    projectId: props.projectId,
    type: contractChangeTypeEnum.ENUM.CONTRACT_INFO.V,
    projectUpdateDTOParam: {
      baseUpdateDTOParam: baseRef.value.form,
      businessUpdateDTOParam: businessRef.value.form,
      customerUpdateDTOParam: customerRef.value.form,
      userIdList: memberRef.value.checkedList
    }
  }
  try {
    await editContract(submitform)
    // await baseRef.value.fetchDetail()
    // await businessRef.value.fetchDetail()
    // await customerRef.value.fetchDetail()
    // await memberRef.value.fetchMembers()
    ElNotification({ title: '提交成功', type: 'success' })
  } catch (error) {
    console.log('合同信息更新失败', error)
  } finally {
    baseRef.value.resetForm()
    businessRef.value.resetForm()
    customerRef.value.resetForm()
    memberRef.value.fetchMembers()
    isModify.value = false
  }
}
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
