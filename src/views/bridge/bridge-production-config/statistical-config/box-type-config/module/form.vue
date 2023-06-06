<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="500px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="110px">
      <el-form-item label="种类" prop="typeName">
        <el-input v-model.trim="form.typeName" type="text" placeholder="请填写种类" style="width: 270px" />
      </el-form-item>
      <el-form-item label="包含单元编号" prop="inventoryNames">
        <el-tag
          v-for="tag in form.inventoryNames"
          :key="tag"
          style="margin-right: 3px; margin-bottom: 3px"
          closable
          :disable-transitions="false"
          size="medium"
          effect="plain"
          @close="handleClose(tag)"
        >
          {{ tag }}
        </el-tag>
        <el-input
          v-if="inputVisible"
          ref="inputRef"
          v-model="inputValue"
          size="small"
          placeholder="大写字母"
          style="width: 120px"
          @keyup.enter="handleInputConfirm"
          @blur="handleInputConfirm"
        />
        <common-button v-else size="small" @click="showInput"> + 新增 </common-button>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, nextTick } from 'vue'
import { regForm } from '@compos/use-crud'

const formRef = ref()
const defaultForm = {
  id: undefined,
  typeName: undefined,
  inventoryNames: []
}

const inputValue = ref('')
const inputVisible = ref(false)
const inputRef = ref()

const { crud, form } = regForm(defaultForm, formRef)

const rules = {
  typeName: [{ required: true, message: '请填写种类', trigger: 'blur' }],
  inventoryNames: [{ required: true, message: '请填写包含分段编号', trigger: 'blur' }]
}

const handleClose = (tag) => {
  form.inventoryNames.splice(form.inventoryNames.indexOf(tag), 1)
}

const showInput = () => {
  inputVisible.value = true
  nextTick(() => {
    inputRef.value?.input?.focus()
  })
}

const handleInputConfirm = () => {
  if (!/^[A-Z]+$/.test(inputValue.value)) {
    inputValue.value = ''
    return
  } else if (inputValue.value) {
    form.inventoryNames.push(inputValue.value)
  }
  inputVisible.value = false
  inputValue.value = ''
}
</script>
