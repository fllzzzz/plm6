<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="ModifyCancel"
    title="合同信息"
    :wrapper-closable="false"
    size="80%"
  >
  <template #content>
    <div>
      <transition name="el-fade-in">
        <div v-show="showName=='contract'" style="margin-bottom:10px;position:relative;">
          <div style="position:absolute;right:0;z-index:2;">
            <div style="display:flex;border:1px solid #ffe399;border-radius:4px;" class="contractbtns" v-if="!isModify">
              <template v-if="!isModify">
                <template v-if="projectStatus===projectStatusEnum.PROCESS.V">
                  <el-tooltip class="item" effect="dark" content="修改" placement="top">
                    <common-button size="mini" icon="el-icon-edit" plain class="next_btn" @click="isModify=true;" v-permission="permission.edit"/>
                  </el-tooltip>
                  <el-tooltip class="item" effect="dark" content="合同金额变更" placement="top">
                    <common-button size="mini" icon="el-icon-tickets" plain class="next_btn" @click="moneyChange" v-permission="permission.changeAmount"/>
                  </el-tooltip>
                </template>
                <template v-if="projectStatus!==projectStatusEnum.SUSPEND.V && projectStatus!==projectStatusEnum.SETTLED.V">
                  <el-tooltip class="item" effect="dark" content="项目结算" placement="top">
                    <common-button  size="mini" icon="el-icon-money" plain class="next_btn" @click="confirmSettle" v-permission="permission.settle"/>
                  </el-tooltip>
                  <el-tooltip class="item" effect="dark" content="变更签证" placement="top">
                    <common-button  size="mini" icon="el-icon-money" plain class="next_btn" @click="variationChange" v-permission="permission.variationChange"/>
                  </el-tooltip>
                </template>
                <el-tooltip class="item" effect="dark" content="打印" placement="top">
                  <common-button size="mini" icon="el-icon-printer" plain class="next_btn" v-permission="permission.print"/>
                </el-tooltip>
                <el-tooltip class="item" effect="dark" content="下载" placement="top">
                  <common-button size="mini" icon="el-icon-download" plain class="next_btn" v-permission="permission.download"/>
                </el-tooltip>
              </template>
            </div>
            <div v-else>
              <common-button size="mini" @click="ModifyCancel">取消</common-button>
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
      <money-form ref="moneyRef" :audit-status="auditStatus" :project-id="projectId" v-model="moneyVisible" :detail-info="baseInfoValue" @success="handleClose"/>
      <!-- 结算填报 -->
      <settle-form ref="settleRef" :audit-status="auditStatus" :project-id="projectId" v-model="settleVisible" :detail-info="baseInfoValue" @success="handleClose"/>
      <!-- 变更签证 -->
      <variation-order ref="variationRef" :audit-status="auditStatus" :project-id="projectId" v-model="variationVisible" :detail-info="baseInfoValue" @success="handleClose"/>
    </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, defineEmits, watch } from 'vue'
import { ElMessage, ElTabs, ElTabPane, ElMessageBox, ElNotification } from 'element-plus'
import { contractChangeTypeEnum } from '@enum-ms/contract'
import baseInfo from './base'
import businessInfo from './business'
import customerInfo from './customer'
import members from './members'
import moneyForm from './money-form'
import settleForm from './settle-form'
import variationOrder from './variation-order'
import changeAuditLog from './change-audit-log'
import { editContract } from '@/api/contract/project'
import { projectStatusEnum } from '@enum-ms/contract'
import useVisible from '@compos/use-visible'
import { judgeSameValue } from './judgeSameValue'
import { projectListPM as permission } from '@/page-permission/contract'

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
  },
  modelValue: {
    type: Boolean,
    require: true
  }
})
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const showName = ref('contract')
const activeName = ref('baseInfo')
const isModify = ref(false)
const moneyVisible = ref(false)
const settleVisible = ref(false)
const variationVisible = ref(false)
const submitLoading = ref(false)
const auditStatus = ref()
const baseRef = ref()
const businessRef = ref()
const customerRef = ref()
const memberRef = ref()
const baseInfoValue = ref()
const moneyRef = ref()
const settleRef = ref()
const variationRef = ref()

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      showName.value = 'contract'
      activeName.value = 'baseInfo'
    }
  },
  { deep: true, immediate: true }
)

function confirmSettle() {
  baseInfoValue.value = baseRef.value.detail
  if (props.projectStatus === projectStatusEnum.PROCESS.V) {
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

function variationChange() {
  variationVisible.value = true
  baseInfoValue.value = baseRef.value.detail
}

function ModifyCancel() {
  if (isModify.value) {
    baseRef.value.resetForm()
    businessRef.value.resetForm()
    customerRef.value.resetForm()
    memberRef.value.fetchMembers()
    isModify.value = false
  }
  activeName.value = 'baseInfo'
  handleClose()
}

const validateData = [
  { name: 'baseInfo', label: '基础信息' },
  { name: 'businessInfo', label: '商务信息' },
  { name: 'customerInfo', label: '客户信息' }
]

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
  const baseChange = judgeSameValue(baseRef.value.form, baseRef.value.detail)
  const businessChange = judgeSameValue(businessRef.value.form, businessRef.value.detail)
  const contentChange = judgeSameValue(businessRef.value.form.projectContent, businessRef.value.detail.projectContent)
  const structureChange = judgeSameValue(businessRef.value.form.structureList, businessRef.value.detail.structureList)
  const profiledPlateChange = judgeSameValue(businessRef.value.form.profiledPlateList, businessRef.value.detail.profiledPlateList)
  const pressureBearingPlateChange = judgeSameValue(businessRef.value.form.pressureBearingPlateList, businessRef.value.detail.pressureBearingPlateList)
  const trussFloorPlateChange = judgeSameValue(businessRef.value.form.trussFloorPlateList, businessRef.value.detail.trussFloorPlateList)
  const sandwichBoardChange = judgeSameValue(businessRef.value.form.sandwichBoardList, businessRef.value.detail.sandwichBoardList)
  const customerChange = judgeSameValue(customerRef.value.form, customerRef.value.detail)
  const userChange = judgeSameValue(memberRef.value.checkedList, memberRef.value.originUserList)
  if (baseChange && businessChange && customerChange && userChange && contentChange && structureChange && profiledPlateChange && pressureBearingPlateChange && trussFloorPlateChange && sandwichBoardChange) {
    ElMessage.error('项目未改动，请修改后提交')
    return
  }
  const submitForm = {
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
    await editContract(submitForm)
    ElNotification({ title: '提交成功', type: 'success' })
    baseRef.value.resetForm()
    businessRef.value.resetForm()
    customerRef.value.resetForm()
    memberRef.value.fetchMembers()
    isModify.value = false
    handleClose()
  } catch (error) {
    console.log('合同信息更新失败', error)
  }
}
</script>

<style lang="scss" scoped>
::v-deep(el-tabs__content) {
  height: calc(100vh - 100px);
    overflow: auto;
}
::v-deep(.contractbtns .next_btn){
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
