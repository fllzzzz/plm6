<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="数量变更"
    :wrapper-closable="false"
    size="60%"
  >
    <template #titleRight>
      <common-button
        type="primary"
        size="mini"
        @click="onSubmit"
      >确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="130px">
        <el-tag size="medium" type="info" effect="dark">构件信息</el-tag>
        <div style="display: flex; width: 100%">
          <el-form-item label="名称">
            <div style="width:100px">{{ detailInfo.name }}</div>
          </el-form-item>
          <el-form-item label="编号">
            <div style="width:100px">{{ detailInfo.serialNumber }}</div>
          </el-form-item>
          <el-form-item label="材质" prop="material">
            <div style="width: 100px">
              <div style="width:100px">{{ detailInfo.material }}</div>
            </div>
          </el-form-item>
        </div>
        <div style="display: flex; width: 100%">
          <el-form-item label="单净重(kg)" prop="netWeight">
            <div style="width:100px">{{ detailInfo.netWeight }}</div>
          </el-form-item>
          <el-form-item label="单毛重(kg)" prop="grossWeight">
            <div style="width:100px">{{ detailInfo.grossWeight }}</div>
          </el-form-item>
          <el-form-item label="规格" prop="specification">
            <div style="width:100px">{{ detailInfo.specification }}</div>
          </el-form-item>
        </div>
        <div style="display: flex; width: 100%">
          <el-form-item label="面积(㎡)" prop="surfaceArea">
            <div style="width:100px">{{ detailInfo.surfaceArea }}</div>
          </el-form-item>
          <el-form-item label="长度(mm)" prop="length">
            <div style="width:100px">{{ detailInfo.length }}</div>
          </el-form-item>
           <el-form-item label="图号" prop="drawingNumber">
            <div style="width:100px">{{ detailInfo.drawingNumber }}</div>
          </el-form-item>
        </div>
        <div style="display: flex; width: 100%">
          <el-form-item label="清单数量">
            <div style="width:100px">{{ detailInfo.quantity }}</div>
          </el-form-item>
          <el-form-item label="生产数量">
            <div style="width:100px">{{ detailInfo.productionQuantity }}</div>
          </el-form-item>
          <el-form-item label="发运数量">
            <div style="width:100px">{{ transportedNum  }}</div>
          </el-form-item>
        </div>
        <div style="display: flex">
          <el-form-item label="变更后清单数量" prop="quantity">
            <div style="width: 270px">
              <el-input-number
                v-model.number="form.quantity"
                :min="transportedNum?transportedNum :0"
                :max="maxNumber"
                :step="1"
                step-strictly
                placeholder=""
                controls-position="right"
                style="width: 200px"/>
            </div>
          </el-form-item>
        </div>
        <el-tag size="medium" type="info" effect="dark">变更原因及附件</el-tag>
        <el-form-item label="原因类型" prop="reasonId">
          <changeRemarkSelect v-model="form.reasonId" clearable/>
        </el-form-item>
        <el-form-item label="原因描述" prop="changeRemark">
          <el-input
            v-model.trim="form.changeRemark"
            type="textarea"
            :autosize="{ minRows: 3, maxRows: 6 }"
            :maxlength="200"
            placeholder="请填写原因描述"
            style="width: 320px"/>
        </el-form-item>
        <el-form-item label="附件上传" prop="attachmentFiles">
          <upload-list
            ref="uploadRef"
            :show-download="false"
            :file-classify="fileClassifyEnum.CHANGE_LIST_ATT.V"
            v-model:files="form.attachmentFiles"
            empty-text="暂未附件"
          />
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, defineEmits, nextTick, watch } from 'vue'
import { fileClassifyEnum } from '@enum-ms/file'
import { ElNotification } from 'element-plus'
import useVisible from '@compos/use-visible'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import changeRemarkSelect from '@comp-base/change-reason-select'
import uploadList from '@comp/file-upload/UploadList.vue'
import { numChange, artifactInfo } from '@/api/plan/technical-manage/artifact-tree'

const formRef = ref()
const maxNumber = 999999999
const defaultForm = {
  quantity: undefined,
  reasonId: undefined,
  changeRemark: undefined,
  attachmentFiles: []
}
const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const uploadRef = ref()
const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

watch(
  () => props.modelValue,
  (val) => {
    if (val) {
      resetForm()
    }
  },
  { deep: true, immediate: true }
)

const validateNum = (rule, value, callback) => {
  if (!value) {
    callback(new Error('变更后数量必须大于0'))
  } else if (value === props.detailInfo.quantity) {
    callback(new Error('数量未改动'))
  } else {
    callback()
  }
}

const rules = {
  quantity: [{ required: true, validator: validateNum, trigger: 'change' }],
  reasonId: [{ required: true, message: '请选择变更原因', trigger: 'change' }],
  changeRemark: [{ required: true, max: 200, message: '不能超过 200 个字符', trigger: 'blur' }]
}

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const transportedNum = ref()

function resetForm() {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  form.value.quantity = props.detailInfo.quantity
  getInfo(props.detailInfo)
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
}

async function getInfo(data) {
  transportedNum.value = 0
  try {
    const data = await artifactInfo({ serialNumber: data.serialNumber, areaId: data.areaId })
    transportedNum.value = data.shipQuantity ? data.shipQuantity : 0
  } catch (error) {
    console.log('获取构件信息', error)
  }
}

useWatchFormValidate(formRef, form)

function handleSuccess() {
  ElNotification({ title: '更改成功', type: 'success' })
  emit('success')
  handleClose()
}

async function onSubmit(val) {
  try {
    await formRef.value.validate()
    form.value.id = props.detailInfo.id
    form.value.attachments = form.value.attachmentFiles && form.value.attachmentFiles.length > 0 ? form.value.attachmentFiles.map((v) => v.id) : []
    await numChange(form.value)
    handleSuccess()
  } catch (e) {
    console.log('修改构件数量', e)
  }
}
</script>
<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
::v-deep(.el-dialog__body) {
  padding: 10px 20px;

  .el-step {
    .el-step__icon {
      width: 20px;
      height: 20px;
      font-size: 12px;
    }
    .el-step__title {
      font-size: 13px;
    }
  }
}
.tree-form {
  ::v-deep(.el-drawer__header) {
    margin-bottom: 0;
  }
}
.item-name {
  padding: 8px 16px;
  background-color: #ecf8ff;
  border-radius: 4px;
  border-left: 5px solid #50bfff;
  margin: 5px 0;
  margin-left: 5px;
  width: 150px;
}
.table-form {
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 5px;
  }
}
</style>

