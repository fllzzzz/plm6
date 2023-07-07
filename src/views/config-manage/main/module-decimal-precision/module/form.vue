<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="520px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
      <el-form-item label="模块" prop="menuId">
        <common-select
          v-model="form.menuId"
          :options="props.menuArr"
          :disabled-val="disabledId"
          type="other"
          :data-structure="{ key: 'id', label: 'name', value: 'id' }"
          placeholder="模块"
          style="width: 200px"
        />
      </el-form-item>
      <el-form-item label="小数类型">
        <span>单价</span>
      </el-form-item>
      <el-form-item label="小数精度" prop="scale">
        <el-input-number
          v-model.number="form.scale"
          :min="3"
          :max="7"
          :precision="0"
          :step="1"
          placeholder="小数精度"
          style="width: 200px"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps } from 'vue'
import { regForm } from '@compos/use-crud'

const props = defineProps({
  menuArr: {
    type: Array,
    default: () => []
  },
  disabledId: {
    type: Array,
    default: () => []
  }
})

const formRef = ref()
const defaultForm = {
  menuId: undefined,
  scale: undefined
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const rules = {
  menuId: [{ required: true, message: '请选择模块', trigger: 'change' }],
  scale: [{ required: true, message: '请输入小数精度', trigger: 'change' }]
}

CRUD.HOOK.beforeSubmit = () => {
  form.list = [{ menuId: form.menuId, scale: form.scale }]
}
</script>
