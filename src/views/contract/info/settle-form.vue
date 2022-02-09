<template>
  <common-dialog
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="项目结算"
    :center="false"
    :close-on-click-modal="false"
  >
    <template #title>
      <div class="dialog-title">
        <span class="title-left">项目结算</span>
        <el-tag v-if="auditStatus" size="medium" :type="auditStatus==auditTypeEnum.ENUM.REJECT.V?'info':(auditStatus==auditTypeEnum.ENUM.PASS.V?'success':'warning')">
          {{ auditStatus==auditTypeEnum.ENUM.REJECT.V?'已驳回':(auditStatus==auditTypeEnum.ENUM.PASS.V?'已通过':'审核中') }}
        </el-tag>
        <span style="position:absolute;right:20px;">
          <template v-if="auditStatus">
            <common-button v-if="auditStatus==auditTypeEnum.ENUM.AUDITING.V && showType==='audit'" size="small" type="info" @click="passConfirm(auditTypeEnum.ENUM.REJECT.V)">驳回</common-button>
            <common-button v-if="auditStatus==auditTypeEnum.ENUM.AUDITING.V && showType==='audit'" size="small" type="success" @click="passConfirm(auditTypeEnum.ENUM.PASS.V)">通过</common-button>
          </template>
          <template v-else>
            <common-button type="primary" size="small" @click="onSubmit">提交</common-button>
          </template>
          <common-button size="small"  @click="handleClose">关闭</common-button>
        </span>
      </div>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="150px">
      <el-form-item label="项目" prop="serialNumber">
        <span v-if="!auditStatus" class="project-name">{{ projectNameFormatter(detailInfo) }}</span>
        <template v-else>
          <span class="project-name" v-if="detailInfo.project">{{ projectNameFormatter(detailInfo.project) }}</span>
        </template>
      </el-form-item>
      <el-form-item label="合同金额" prop="serialNumber">
        <span>{{ toThousand(detailInfo.contractAmount) }}</span>
      </el-form-item>
      <el-form-item label="结算金额（元）" prop="settlementAmount">
        <template v-if="!auditStatus">
          <el-input-number
            v-show-thousand
            v-model="form.settlementAmount"
            :max="9999999999"
            :min="0"
            :step="100"
            :precision="DP.YUAN"
            controls-position="right"
            placeholder="结算金额（元）"
            style="width: 320px;"
          />
          <div>{{form.settlementAmount? digitUppercase(form.settlementAmount): ''}}</div>
        </template>
        <template v-else>
          <span>{{ toThousand(detailInfo.settlementAmount) }}</span>
          <div>{{ digitUppercase(detailInfo.settlementAmount) }}</div>
        </template>
      </el-form-item>
      <el-form-item label="结算差异(元)" prop="newAmount">
        <span v-if="!auditStatus">{{ isNotBlank(newAmount)? toThousand(newAmount):'' }}</span>
        <span v-else>{{ toThousand(detailInfo.settlementAmount-detailInfo.contractAmount) }}</span>
      </el-form-item>
      <el-form-item label="结算日期" prop="changeDate">
        <el-date-picker
          v-if="!auditStatus"
          v-model="form.changeDate"
          type="date"
          value-format="x"
          placeholder="结算日期"
          style="width: 320px;"
          :disabledDate="(date) => { return date.getTime() < new Date().getTime() - 1 * 24 * 60 * 60 * 1000 }"
        />
        <span v-else>{{ detailInfo.changeDate? parseTime(detailInfo.changeDate,'{y}-{m}-{d}'): '-' }}</span>
      </el-form-item>
      <el-form-item label="负责人" prop="userList">
        <user-dept-cascader
          v-if="!auditStatus"
          v-model="form.userList"
          multiple
          filterable
          clearable
          show-all-levels
          placeholder="负责人"
          style="width: 320px;"
        />
        <template v-else>
          <span v-for="item in detailInfo.leaderList" :key="item.id">{{item.name}}</span>
        </template>
      </el-form-item>
      <el-form-item label="附件">
        <upload-btn v-if="!auditStatus" ref="uploadRef" v-model:files="form.attachments" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" />
        <template v-if="auditStatus && detailInfo.attachmentList && detailInfo.attachmentList.length>0">
          <div v-for="item in detailInfo.attachmentList" :key="item.id">{{item.name}}
            <export-button :params="{id: item.id}"/>
          </div>
        </template>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps, computed, watch, defineEmits, nextTick } from 'vue'
import { auditTypeEnum, contractChangeTypeEnum } from '@enum-ms/contract'
import { fileClassifyEnum } from '@enum-ms/file'
import useVisible from '@compos/use-visible'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import UploadBtn from '@comp/file-upload/UploadBtn'
import { DP } from '@/settings/config'
import { editContract, confirmContract } from '@/api/contract/project'
import { isNotBlank } from '@data-type/index'
import { ElNotification, ElMessageBox } from 'element-plus'
import { parseTime } from '@/utils/date'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import { toThousand } from '@data-type/number'
import { projectNameFormatter } from '@/utils/project'
import ExportButton from '@comp-common/export-button/index.vue'
import { digitUppercase } from '@/utils/data-type/number'

const props = defineProps({
  projectId: [Number, String],
  auditStatus: [Number, String],
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  },
  showType: {
    type: String,
    default: undefined
  }
})

const defaultForm = {
  id: undefined,
  projectId: '',
  settlementAmount: undefined,
  changeDate: '',
  userList: [],
  attachments: undefined,
  attachmentIds: undefined
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const formRef = ref()
const uploadRef = ref()
const validateLength = (rule, value, callback) => {
  if (!value.length) {
    callback(new Error('请选择负责人'))
  } else {
    callback()
  }
}

const validateMoney = (rule, value, callback) => {
  if (value <= 0) {
    callback(new Error('结算金额必须大于0'))
  } else {
    callback()
  }
}

const rules = {
  settlementAmount: { required: true, validator: validateMoney, trigger: 'change' },
  userList: { required: true, validator: validateLength, trigger: 'change' },
  changeDate: { required: true, message: '请选择结算日期', trigger: 'blur' }
}
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const newAmount = computed(() => {
  return props.detailInfo.contractAmount && form.value.settlementAmount ? form.value.settlementAmount - props.detailInfo.contractAmount : ''
})

watch(
  () => visible.value,
  (val) => {
    if (val) {
      if (!props.auditStatus) {
        resetForm()
      }
    }
  },
  { deep: true, immediate: true }
)

function resetForm(data) {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  if (data && Object.keys(data).length > 0) {
    form.value = data
  } else {
    form.value = JSON.parse(JSON.stringify(defaultForm))
  }
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
  useWatchFormValidate(formRef, form)
}

async function onSubmit() {
  try {
    const valid = await formRef.value.validate()
    if (!valid) {
      return
    }
    form.value.projectId = props.projectId
    form.value.attachmentIds = form.value.attachments ? form.value.attachments.map((v) => v.id) : undefined
    const submitform = {
      type: contractChangeTypeEnum.CONTRACT_SETTLE.V,
      ...form.value
    }
    await editContract(submitform)
    ElNotification({ title: '提交成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    console.log('项目结算失败', error)
  }
}

const inputValid = (val) => {
  if ((!val || !val.trim()) && val !== 0) {
    return '必填'
  }
  if (val.length > 200) {
    return '长度在 1 到 200 个字符'
  }
  return true
}
async function passConfirm(val) {
  try {
    const title = val === auditTypeEnum.PASS.V ? '通过' : '驳回'
    const remarkValue = await ElMessageBox.prompt('请输入审核说明', title, {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      inputType: 'textarea',
      inputValidator: inputValid,
      type: 'warning'
    })
    const submitData = {
      auditStatus: val,
      id: props.detailInfo.id,
      remark: remarkValue.value
    }
    await confirmContract(submitData)
    ElNotification({ title: '提交成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    console.log('审核', error)
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
  ::v-deep(.el-input-number .el-input__inner) {
    text-align: left;
  }
  .title-left {
    display: flex;
    align-items: center;
    position: relative;
    padding-left: 10px;
    margin-right: 15px;
    box-sizing: border-box;
  }
  .title-left::before {
    content: "";
    width: 4px;
    height: 15px;
    border-radius: 10px;
    background: #1890ff;
    position: absolute;
    top: 50%;
    left: 0;
    transform: translateY(-50%);
}
</style>
