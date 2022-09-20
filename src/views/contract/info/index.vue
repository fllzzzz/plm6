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
              <template v-if="!isModify && btnShow">
                <template v-if="projectStatus===projectStatusEnum.PROCESS.V">
                  <el-tooltip class="item" effect="dark" content="修改" placement="top">
                    <common-button size="mini" icon="el-icon-edit" plain class="next_btn" @click="isModify=true;" v-permission="permission.edit"/>
                  </el-tooltip>
                  <el-tooltip class="item" effect="dark" content="合同金额变更" placement="top">
                    <common-button size="mini" icon="el-icon-tickets" plain class="next_btn" @click="moneyChange" v-permission="permission.changeAmount"/>
                  </el-tooltip>
                </template>
                <el-tooltip class="item" effect="dark" content="下载" placement="top">
                  <export-button :fn="downloadProjectInfo" class="next_btn" :params="{ projectId: projectId }" v-permission="permission.download"/>
                </el-tooltip>
              </template>
            </div>
            <div v-else>
              <common-button size="mini" @click="ModifyCancel">取消</common-button>
              <common-button :loading="submitLoading" size="mini" type="success" @click="validateSubmit">保存</common-button>
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
      <!-- 提交确认弹窗 -->
      <common-dialog
        append-to-body
        v-model="changeVisible"
        top="10vh"
        width="600px"
        :before-close="changeClose"
        title="合同变更"
        :center="false"
        :close-on-click-modal="false"
      >
        <template #titleRight>
          <common-button type="primary" size="small" @click="onSubmit">提交</common-button>
        </template>
        <el-form ref="changeFormRef" :model="changeForm" :rules="rules" size="small" label-width="150px">
          <el-form-item label="变更原因描述" prop="changeDesc">
            <el-input
              v-model.trim="changeForm.changeDesc"
              type="textarea"
              :autosize="{ minRows: 2, maxRows: 8}"
              placeholder="请填写描述"
              style="width: 320px;"
              :maxlength="200"
              show-word-limit
            />
          </el-form-item>
          <el-form-item label="变更日期" prop="changeDate">
            <el-date-picker
              v-model="changeForm.changeDate"
              type="date"
              value-format="x"
              placeholder="变更日期"
              style="width: 320px;"
            />
          </el-form-item>
          <el-form-item label="操作人" prop="changeDate">
            <span>{{user.name}}</span>
          </el-form-item>
        </el-form>
      </common-dialog>
      <!-- 金额变更 -->
      <money-form ref="moneyRef" :audit-status="auditStatus" :project-id="projectId" v-model="moneyVisible" :detail-info="baseInfoValue" @success="handleClose" :member-list="memberList"/>
      <!-- 结算填报 -->
      <settle-form ref="settleRef" :audit-status="auditStatus" :project-id="projectId" v-model="settleVisible" :detail-info="baseInfoValue" @success="handleClose" :member-list="memberList"/>
      <!-- 变更签证 -->
      <variation-order ref="variationRef" :audit-status="auditStatus" :project-id="projectId" v-model="variationVisible" :detail-info="baseInfoValue" @success="handleClose" :member-list="memberList"/>
    </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, defineEmits, watch, nextTick } from 'vue'
import { ElMessage, ElTabs, ElTabPane, ElNotification } from 'element-plus'

import { mapGetters } from '@/store/lib'
import { contractChangeTypeEnum } from '@enum-ms/contract'
import { editContract, downloadProjectInfo } from '@/api/contract/project'
import { projectStatusEnum } from '@enum-ms/contract'
import useVisible from '@compos/use-visible'
import { judgeSameValue } from './judgeSameValue'
import { projectListPM as permission } from '@/page-permission/contract'
import { compareArrayValue } from './judgeSameValue'

import baseInfo from './base'
import businessInfo from './business'
import customerInfo from './customer'
import members from './members'
import moneyForm from './money-form'
import settleForm from './settle-form'
import variationOrder from './variation-order'
import ExportButton from '@comp-common/export-button/index.vue'

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
  },
  btnShow: {
    type: Boolean,
    default: true
  }
})
const { user } = mapGetters('user')
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
const memberList = ref([])
const moneyRef = ref()
const settleRef = ref()
const variationRef = ref()
const changeVisible = ref(false)
const changeFormRef = ref()
const changeForm = ref({
  changeDesc: undefined,
  changeDate: new Date().getTime()
})

const rules = {
  changeDesc: [
    { required: true, message: '请填写变更原因描述', trigger: 'blur' }
  ],
  changeDate: [{ required: true, message: '请选择日期', trigger: 'change' }]
}

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

function moneyChange() {
  moneyVisible.value = true
  baseInfoValue.value = baseRef.value.detail
  memberList.value = memberRef.value.checkedList || []
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

function resetChangeForm() {
  if (changeFormRef.value) {
    nextTick(() => {
      changeFormRef.value.resetFields()
    })
  }
  if (changeFormRef.value) {
    nextTick(() => {
      changeFormRef.value.clearValidate()
    })
  }
}

const validateData = [
  { name: 'baseInfo', label: '基础信息' },
  { name: 'businessInfo', label: '商务信息' },
  { name: 'customerInfo', label: '客户信息' }
]

async function validateSubmit() {
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
  const customerChange = judgeSameValue(customerRef.value.form, customerRef.value.detail)
  const userChange = compareArrayValue(memberRef.value.checkedList, memberRef.value.originUserList, false)
  if (baseChange && businessChange && customerChange && userChange) {
    ElMessage.error('项目未改动，请修改后提交')
    return
  }
  baseRef.value.form.attachments = baseRef.value.form.attachmentFiles.length > 0 ? baseRef.value.form.attachmentFiles.map(v => v.id) : []
  changeVisible.value = true
  resetChangeForm()
  changeForm.value = {
    projectId: props.projectId,
    type: contractChangeTypeEnum.ENUM.CONTRACT_INFO.V,
    projectUpdateDTOParam: {
      baseUpdateDTOParam: baseRef.value.form,
      businessUpdateDTOParam: businessRef.value.form,
      customerUpdateDTOParam: customerRef.value.form,
      userIdList: memberRef.value.checkedList
    },
    ...changeForm.value
  }
}

async function onSubmit() {
  const valid = await changeFormRef.value.validate()
  if (!valid) {
    return
  }
  try {
    await editContract(changeForm.value)
    ElNotification({ title: '提交成功', type: 'success' })
    baseRef.value.resetForm()
    businessRef.value.resetForm()
    customerRef.value.resetForm()
    memberRef.value.fetchMembers()
    isModify.value = false
    changeClose()
    handleClose()
  } catch (error) {
    console.log('合同信息更新失败', error)
  }
}

function changeClose() {
  changeVisible.value = false
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
