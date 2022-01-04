<template>
  <common-dialog
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="合同金额变更"
    :center="false"
  >
    <template #title>
      <div class="dialog-title">
        <span class="title-left">合同金额变更</span>
        <common-button v-if="auditStatus" size="mini" :type="auditStatus==auditTypeEnum.ENUM.REJECT.V?'info':(auditStatus==auditTypeEnum.ENUM.PASS.V?'success':'warning')">
          {{ auditStatus==auditTypeEnum.ENUM.REJECT.V?'已驳回':(auditStatus==auditTypeEnum.ENUM.PASS.V?'已通过':'审核中') }}
        </common-button>
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
      <el-form-item label="合同编号" prop="serialNumber">
        <el-input
          v-if="!auditStatus"
          v-model="contractName"
          style="width: 320px;"
          disabled
        />
        <span v-else>{{ detailInfo.project? detailInfo.project.serialNumber+detailInfo.project.shortName: '-'}}</span>
      </el-form-item>
      <el-form-item label="合同金额" prop="serialNumber">
         <span v-if="!auditStatus">{{ contractInfo.contractAmount }}</span>
         <span v-else>{{ detailInfo.project.contractAmount }}</span>
      </el-form-item>
      <el-form-item label="变更内容" prop="changeContent">
        <el-input
          v-if="!auditStatus"
          v-model="form.changeContent"
          placeholder="变更内容"
          style="width: 320px;"
        />
        <span v-else>{{ form.changeContent }}</span>
      </el-form-item>
      <el-form-item label="变更金额" prop="changeMoney">
        <el-input-number
          v-if="!auditStatus"
          v-model="form.changeMoney"
          :max="9999999999"
          :min="-contractInfo.contractAmount"
          :step="100"
          :precision="DP.YUAN"
          controls-position="right"
          placeholder="变更金额(元)"
          style="width: 320px;"
        />
        <span v-else>{{ form.changeAmount }}</span>
      </el-form-item>
      <el-form-item label="变更后合同金额(元)" prop="newAmount">
        <el-input-number
          v-if="!auditStatus"
          v-model="newAmount"
          :max="9999999999"
          :min="-9999999999"
          :precision="DP.YUAN"
          controls-position="right"
          placeholder="变更合同金额"
          disabled
          style="width: 320px;"
        />
        <span v-else>{{ newAmount }}</span>
      </el-form-item>
      <el-form-item label="变更日期" prop="changeDate">
        <el-date-picker
          v-if="!auditStatus"
          v-model="form.changeDate"
          type="date"
          value-format="x"
          placeholder="变更日期"
          style="width: 320px;"
        />
        <span v-else>{{ form.changeDate?parseTime(form.changeDate,'{y}-{m}-{d}'):'-' }}</span>
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
        <span v-else>{{ form.userList }}</span>
      </el-form-item>
      <el-form-item label="描述" prop="changeDesc">
        <el-input
          v-if="!auditStatus"
          v-model="form.changeDesc"
          type="textarea"
          :autosize="{ minRows: 2, maxRows: 8}"
          placeholder="请填写描述"
          style="width: 320px;"
          :maxlength="500"
        />
        <span v-else>{{ form.changeDesc }}</span>
      </el-form-item>
      <el-form-item label="附件">
        <upload-btn v-if="!auditStatus" ref="uploadRef" v-model:files="form.attachments" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" />
        <span v-else />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps, computed, watch, defineEmits } from 'vue'
import { auditTypeEnum, contractChangeTypeEnum } from '@enum-ms/contract'
import { fileClassifyEnum } from '@enum-ms/file'
import useVisible from '@compos/use-visible'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import UploadBtn from '@comp/file-upload/UploadBtn'
import { DP } from '@/settings/config'
import { editContract } from '@/api/contract/project'
import { isNotBlank } from '@data-type/index'
import { ElNotification } from 'element-plus'
import { parseTime } from '@/utils/date'

const props = defineProps({
  projectId: [Number, String],
  auditStatus: [Number, String],
  modelValue: {
    type: Boolean,
    require: true
  },
  contractInfo: {
    type: Object,
    default: () => {}
  },
  showType: {
    type: String,
    default: undefined
  },
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

const defaultForm = {
  id: undefined,
  projectId: '',
  changeMoney: undefined,
  changeAmount: undefined,
  changeContent: undefined,
  changeDate: '',
  userList: [],
  changeDesc: undefined,
  attachments: undefined,
  attachmentIds: undefined
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const formRef = ref()
const uploadRef = ref()
const contractName = ref()
const validateLength = (rule, value, callback) => {
  if (!value.length) {
    callback(new Error('请选择负责人'))
  } else {
    callback()
  }
}

const rules = {
  changeMoney: { required: true, message: '请填写变更金额', trigger: 'change' },
  userList: { required: true, validator: validateLength, trigger: 'change' },
  changeDate: { required: true, message: '请选择变更日期', trigger: 'blur' }
}
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

// watch(
//   () => visible.value,
//   (val) => {
//     if (val) {
//       resetForm()
//     }
//   },
//   { deep: true, immediate: true }
// )

watch(
  () => props.contractInfo,
  (val) => {
    contractName.value = isNotBlank(val) ? props.contractInfo.serialNumber + ' ' + props.contractInfo.shortName : ''
  },
  { deep: true, immediate: true }
)

const newAmount = computed(() => {
  return props.contractInfo.contractAmount && form.value.changeMoney ? props.contractInfo.contractAmount + form.value.changeMoney : undefined
})

// function resetForm(data) {
//   if (formRef.value) {
//     formRef.value.resetFields()
//   }
//   let formkey
//   if (data && Object.keys(data).length > 0) {
//     formkey = data
//   } else {
//     formkey = JSON.parse(JSON.stringify(defaultForm))
//   }
//   const crudFrom = form.value
//   for (const key in crudFrom) {
//     crudFrom[key] = undefined
//   }
//   for (const key in formkey) {
//     crudFrom[key] = formkey[key]
//   }
// }

async function onSubmit() {
  const valid = await formRef.value.validate()
  if (!valid) {
    return
  }
  form.value.projectId = props.projectId
  form.value.attachmentIds = form.value.attachments ? form.value.attachments.map((v) => v.id) : undefined
  form.value.changeAmount = newAmount
  const submitform = {
    type: contractChangeTypeEnum.ENUM.CONTRACT_AMOUNT.V,
    ...form.value
  }
  try {
    await editContract(submitform)
    ElNotification({ title: '提交成功', type: 'success' })
  } catch (error) {
    console.log('金额变更失败', error)
  } finally {
    handleClose()
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
