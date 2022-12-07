<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="dialogVisible"
    :title="`${isEdit ? '编辑' : '新增'}切割详情`"
    width="500px"
  >
    <template #titleRight>
      <common-button type="primary" size="mini" @click="submit">提交</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="170px">
      <el-form-item label="孔径数值范围（毫米）" prop="numBoreDiameter">
        <common-input-number
          v-model="form.minBoreDiameter"
          :step="1"
          :min="0"
          :precision="2"
          clearable
          :controls="false"
          size="mini"
          class="input-underline"
          placeholder="最小孔径(毫米)"
          style="width: 45%"
        />
        <span> ~ </span>
        <common-input-number
          v-model="form.maxBoreDiameter"
          :step="1"
          :precision="2"
          class="input-underline"
          :controls="false"
          size="mini"
          clearable
          placeholder="最大孔径(毫米)"
          style="width: 45%"
        />
      </el-form-item>
      <el-form-item label="单价（元/个）" prop="unitPrice">
        <common-input-number
          v-model="form.unitPrice"
          :step="1"
          :min="0"
          :precision="2"
          clearable
          class="input-underline"
          :controls="false"
          size="mini"
          placeholder="单价"
          style="width: 100%"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import useVisible from '@compos/use-visible'
import { add, editGet as edit } from '@/api/mes/production-config/drill-detail'
import { ref, defineProps, defineEmits } from 'vue'
import { ElNotification } from 'element-plus'

const emit = defineEmits(['update:visible', 'refresh'])

const formRef = ref()
const submitLoading = ref(false)

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  isEdit: {
    type: Boolean,
    default: false
  },
  layingOffRowId: {
    type: Number
  },
  formData: {
    type: Object,
    default: () => {}
  }
})
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })
const form = ref({
  id: undefined,
  unitPrice: undefined,
  numBoreDiameter: undefined
})

function showHook() {
  // 编辑赋值
  if (props.isEdit) {
    form.value = props.formData
  } else {
    form.value = {}
  }
}

const validateNumBoreDiameter = (rule, value, callback) => {
  if (!form.value.minBoreDiameter || !form.value.maxBoreDiameter) {
    callback(new Error('请填写孔径'))
  } else if (form.value.maxBoreDiameter < form.value.minBoreDiameter) {
    callback(new Error('最大孔径不得小于最小孔径'))
  }
  callback()
}
const rules = {
  unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  numBoreDiameter: [{ required: true, validator: validateNumBoreDiameter, message: '请填写孔径', trigger: 'blur' }]
}

async function submit() {
  try {
    const valid = await formRef.value.validate()
    if (valid) {
      submitLoading.value = true
      // 新增
      if (!props.isEdit) {
        await add({ ...form.value, boreholeConfigId: props.layingOffRowId })
      } else {
        // 编辑
        await edit({ ...form.value, layingOffRowId: props.layingOffRowId })
      }
      ElNotification({
        title: `${props.isEdit ? '编辑' : '新增'}成功`,
        type: 'success'
      })
      handleClose()
      emit('refresh', form.value)
    }
  } catch (error) {
    console.log('切割详情配置失败', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
