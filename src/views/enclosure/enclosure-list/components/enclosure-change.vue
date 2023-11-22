<template>
  <common-dialog title="围护变更" width="650px" v-model="dlgVisible" :before-close="handleClose">
    <template #titleRight>
      <el-checkbox
        label="强制删除"
        size="mini"
        class="filter-item force-del"
        v-model="forceDel"
        border
        style="border-color: #f56c6c"
      />
      <common-button type="primary" size="mini" :loading="loading" @click="onSubmit">确认</common-button>
    </template>
    <el-form ref="formRef" label-width="130px" size="small" :model="form" :rules="rules">
      <el-form-item label="名称：">
        {{ detailInfo.name }}
      </el-form-item>
      <el-form-item label="编号：">
        {{ detailInfo.serialNumber }}
      </el-form-item>
      <el-form-item label="版型：">
        {{ detailInfo.plate }}
      </el-form-item>
      <el-form-item label="单长(mm)：">
        {{ detailInfo.length }}
      </el-form-item>
      <el-form-item label="数量：" prop="quantity">
        <el-input-number
          v-model.number="form.quantity"
          controls-position="right"
          style="width: 200px"
          :min="0"
          :max="999999"
          :step="1"
          step-strictly
        ></el-input-number>
      </el-form-item>
      <el-form-item label="原因类型：" prop="reasonId">
        <changeRemarkSelect v-model="form.reasonId" clearable />
      </el-form-item>
      <el-form-item label="原因说明：" prop="changeRemark">
        <el-input
        v-model.trim="form.changeRemark"
          type="textarea"
          :maxlength="200"
          placeholder="请填写原因描述"
          style="width: 320px"
        ></el-input>
      </el-form-item>
      <el-form-item v-if="forceDel">
        <span class="form-item-tip" style="color: red">* 提示：强制删除，未发运的构件皆可删除！请谨慎操作！</span>
      </el-form-item>
    </el-form>
  </common-dialog>
  <router-view></router-view>
</template>
<script setup>
import { defineProps, defineEmits, ref, nextTick, watch } from 'vue'
// import { useRouter } from 'vue-router'
import { ElNotification } from 'element-plus'
import { enclosureListChange } from '@/api/enclosure/enclosure-list/profiled-plate'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import changeRemarkSelect from '@comp-base/change-reason-select'
import useVisible from '@compos/use-visible'

const props = defineProps({
  visible: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

const emit = defineEmits(['update:visible', 'success'])

const formRef = ref()
// const router = useRouter()
const loading = ref(false)
const forceDel = ref(false)
const defaultForm = {
  quantity: undefined,
  reasonId: undefined,
  changeRemark: undefined
}
const form = ref(JSON.parse(JSON.stringify(defaultForm)))

watch(
  () => props.visible,
  (val) => {
    if (val) {
      resetForm()
    }
  },
  { deep: true, immediate: true }
)

const { visible: dlgVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
useWatchFormValidate(formRef, form)

function resetForm() {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  loading.value = false
  form.value.quantity = props.detailInfo.quantity
  form.value.netWeight = props.detailInfo.netWeight
  form.value.grossWeight = props.detailInfo.grossWeight
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
}

const validateNum = (rule, value, callback) => {
  if (!forceDel.value) {
    if (!value) {
      callback(new Error('变更后数量必须大于0'))
    } else {
      callback()
    }
  } else {
    callback()
  }
}

function handleSuccess() {
  ElNotification({ title: '更改成功', type: 'success' })
  emit('success')
  handleClose()
}

const rules = {
  quantity: [{ validator: validateNum, trigger: 'change' }],
  reasonId: [{ required: true, message: '请选择变更原因', trigger: 'change' }],
  changeRemark: [{ required: true, max: 200, message: '不能超过 200 个字符', trigger: 'blur' }]
}

async function onSubmit(val) {
  try {
    await formRef.value.validate()
    loading.value = true
    form.value.id = props.detailInfo.id
    if (forceDel.value) {
      form.value.quantity = 0
    }
    await enclosureListChange(form.value)
    handleSuccess()
  } catch (e) {
    console.log('修改构件信息', e)
  } finally {
    loading.value = false
  }

  // router.push({ name: 'EnclosureChangeApproval' })
}

</script>

<style lang="scss" scoped>
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

::v-deep(.el-checkbox.is-bordered) {
  border-color: #f56c6c !important;
}
//修改选择框的颜色
::v-deep(.el-checkbox__input .el-checkbox__inner, .el-checkbox__input.is-indeterminate .el-checkbox__inner) {
  border-color: #f56c6c !important;
}
::v-deep(.el-checkbox__input.is-checked .el-checkbox__inner, .el-checkbox__input.is-indeterminate .el-checkbox__inner) {
  border-color: #f56c6c !important;
  background-color: #f56c6c !important;
}
::v-deep(.el-checkbox__input .el-checkbox__inner) {
  border-color: #f56c6c !important;
}
::v-deep(.el-checkbox__input.is-focus .el-checkbox__inner) {
  border-color: #f56c6c !important;
}
//修改选中后文本的颜色
::v-deep(.el-checkbox__input + .el-checkbox__label) {
  color: #f56c6c !important;
}
::v-deep(.el-checkbox__input.is-checked + .el-checkbox__label) {
  color: #f56c6c !important;
}
</style>
