<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="构件变更"
    width="650px"
  >
    <template #titleRight>
      <common-button
        type="primary"
        size="mini"
        :loading="loading"
        @click="onSubmit"
      >确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="130px">
        <el-form-item label="名称">
          {{ detailInfo.name }}
        </el-form-item>
        <el-form-item label="编号">
          {{ detailInfo.serialNumber }}
        </el-form-item>
        <el-form-item label="规格" prop="specification">
          {{ detailInfo.specification }}
        </el-form-item>
        <el-form-item label="长度(mm)" prop="length">
          {{ detailInfo.length }}
        </el-form-item>
        <el-form-item label="数量" prop="quantity">
          <el-input-number
            v-model.number="form.quantity"
            :min="transportedNum?transportedNum :0"
            :max="maxNumber"
            :step="1"
            step-strictly
            placeholder=""
            controls-position="right"
            style="width: 200px"/>
        </el-form-item>
        <!-- <el-form-item label="单净重(kg)" prop="netWeight">
          {{ detailInfo.netWeight }}
        </el-form-item>
        <el-form-item label="单毛重(kg)" prop="grossWeight">
          <el-input-number
            v-model.number="form.grossWeight"
            :min="0"
            :max="99999999999"
            :step="1"
            :precision="DP.COM_WT__KG"
            placeholder="单毛重"
            controls-position="right"
            style="width: 220px;"
          />
        </el-form-item>
        <el-form-item label="面积(㎡)" prop="surfaceArea">
          <el-input-number
            v-model.number="form.surfaceArea"
            :min="0"
            :max="99999999999"
            :step="1"
            :precision="DP.COM_AREA__M2"
            placeholder="面积"
            controls-position="right"
            style="width: 220px;"
          />
        </el-form-item>
        <el-form-item label="图号" prop="drawingNumber">
          <el-input
            v-model.trim="form.drawingNumber"
            type="text"
            :maxlength="20"
            placeholder="请输入图号"
            style="width: 220px"/>
        </el-form-item> -->
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
      </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps, defineEmits, nextTick, watch } from 'vue'
import { ElNotification } from 'element-plus'

// import { DP } from '@/settings/config'
import useVisible from '@compos/use-visible'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'

import changeRemarkSelect from '@comp-base/change-reason-select'
import { numChange } from '@/api/plan/technical-manage/artifact-tree'

const formRef = ref()
const maxNumber = 999999999
const defaultForm = {
  quantity: undefined,
  reasonId: undefined,
  changeRemark: undefined
}
const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const loading = ref(false)
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
  loading.value = false
  form.value.quantity = props.detailInfo.quantity
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
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
    loading.value = true
    form.value.id = props.detailInfo.id
    await numChange(form.value)
    handleSuccess()
  } catch (e) {
    console.log('修改构件信息', e)
  } finally {
    loading.value = false
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

