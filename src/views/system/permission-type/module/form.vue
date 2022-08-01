<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="600px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="80px">
     <el-form-item label="类型名称" prop="name">
        <el-input v-model="form.name" style="width: 450px;" placeholder="类型名称" />
      </el-form-item>
      <el-form-item label="类型图标" prop="icon">
        <el-popover
          placement="bottom-start"
          width="450"
          trigger="click"
          v-model:visible="visible"
        >
          <IconSelect ref="iconSelect" @selected="selected" style="width:424px;"/>
          <template #reference>
            <el-input v-model="form.icon" style="width: 450px;" placeholder="点击选择图标" readonly>
              <template #prefix>
                <svg-icon v-if="form.icon" :icon-class="form.icon" class="el-input__icon" style="height: 32px;width: 16px;" />
                <i v-else class="el-icon-search el-input__icon" />
              </template>
            </el-input>
          </template>
        </el-popover>
      </el-form-item>
      <el-form-item label="类型排序" prop="sort">
        <el-input-number v-model.number="form.sort" :min="0" :max="999" controls-position="right" style="width: 450px;" />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import { regForm } from '@compos/use-crud'
import IconSelect from '@comp/IconSelect/index.vue'

const formRef = ref()
const iconSelect = ref()
const visible = ref(false)
const defaultForm = {
  id: undefined,
  name: undefined,
  icon: undefined,
  sort: undefined
}
const { crud, form } = regForm(defaultForm, formRef)

const rules = {
  name: [
    { required: true, message: '请填写名称', trigger: 'blur' },
    { min: 1, max: 10, message: '请填写1-10个字符', trigger: 'blur' }
  ],
  icon: [
    { required: true, message: '请选择图标', trigger: 'change' }
  ]
}

function selected(name) {
  form.icon = name
  visible.value = false
}
</script>
