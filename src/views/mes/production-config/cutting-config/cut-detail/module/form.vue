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
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px">
      <el-form-item label="厚度：" prop="thickness">
        <el-input v-model="form.thickness" clearable placeholder="输入厚度" class="filter-item" style="width: 270px" />
      </el-form-item>
      <el-form-item label="是否开孔：" prop="boolDrillEnum">
        <common-select
          v-model="form.boolDrillEnum"
          :options="whetherEnum.ENUM"
          type="enum"
          clearable
          size="small"
          placeholder="是否开孔"
          class="filter-item"
          style="width: 270px"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import useVisible from '@compos/use-visible'
import { addCutConfigDetail as add, editCutConfigDetail as edit } from '@/api/mes/production-config/unloading-config'
import { ref, defineProps, defineEmits } from 'vue'
import { whetherEnum } from '@enum-ms/common'
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
  cutConfigId: {
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
  thickness: undefined,
  boolDrillEnum: undefined
})

function showHook() {
  // 编辑赋值
  if (props.isEdit) {
    form.value = props.formData
  } else {
    form.value = {}
  }
}

const rules = {
  thickness: [{ required: true, message: '请填写厚度', trigger: 'blur' }],
  boolDrillEnum: [{ required: true, message: '请选择是否开孔', trigger: 'blur' }]
}

async function submit() {
  try {
    const valid = await formRef.value.validate()
    if (valid) {
      submitLoading.value = true
      // 新增
      if (!props.isEdit) {
        await add({ ...form.value, cutConfigId: props.cutConfigId })
      } else {
        // 编辑
        await edit({ ...form.value, cutConfigId: props.cutConfigId })
      }
      ElNotification({
        title: `${props.isEdit ? '编辑' : '新增'}成功`,
        type: 'success'
      })
      handleClose()
      emit('refresh')
    }
  } catch (error) {
    console.log('切割详情配置失败', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
