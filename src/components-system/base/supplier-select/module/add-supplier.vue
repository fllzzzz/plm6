<!-- 供应商:下拉选择框 -->
<template>
  <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px">
    <el-form-item label="物流单位" prop="name">
      <el-input v-model="form.name" maxlength="32" show-word-limit placeholder="请输入供应商名称" />
    </el-form-item>
    <el-form-item label="社会统一代码" prop="socialCode">
      <el-input v-model="form.socialCode" placeholder="请输入社会统一代码" maxlength="50" />
    </el-form-item>
    <!-- <el-form-item label="供应商分类" prop="supplierClass">
      <common-select
        v-model="form.supplierClass"
        :options="supplierClassEnum.ENUM"
        disabled
        type="enum"
        placeholder="请选择供应商分类"
        style="width: 100%"
        size="medium"
        @change="handleSupplierClass"
      />
    </el-form-item> -->
    <div style="text-align: right">
      <common-button :loading="submitLoading" type="primary" size="mini" @click="submit">提 交</common-button>
      <common-button size="mini" @click="handleClose">关闭</common-button>
    </div>
  </el-form>
</template>

<script setup>
import { batchAdd } from '@/api/supply-chain/supplier/manage'
import { ref, defineEmits } from 'vue'
import { useStore } from 'vuex'
import { supplierClassEnum } from '@enum-ms/supplier'

import { ElNotification } from 'element-plus'

const emit = defineEmits(['close'])

const store = useStore()
const formRef = ref()
const submitLoading = ref(false)
const formData = {
  name: '',
  socialCode: '',
  supplierClass: supplierClassEnum.LOGISTICS.V,
  supplierClassification: supplierClassEnum.LOGISTICS.V
}
const form = ref({ ...formData })
const rules = {
  name: [
    { required: true, message: '请输入供应商名称', trigger: 'blur' },
    { min: 2, max: 32, message: '长度在 2 到 32 个字符', trigger: 'blur' }
  ],
  supplierClass: [{ required: true, message: '请选择供应商分类', trigger: 'blur' }]
}

function init() {
  form.value = { ...formData }
}

// eslint-disable-next-line no-unused-vars
function handleSupplierClass(val) {
  let supplierClass
  if (val) {
    val.forEach((v) => {
      supplierClass |= v
    })
  }
  form.value.supplierClassification = supplierClass
}

async function submit() {
  let valid = false
  formRef.value.validate((val) => {
    valid = val
  })
  try {
    if (valid) {
      submitLoading.value = true
      await batchAdd([{ ...form.value }])
      handleClose()
      init()
      ElNotification({ title: '新增成功', type: 'success' })
      store.dispatch('config/fetchSuppliers')
    }
  } catch (error) {
    console.log('新增供应商', error)
  } finally {
    submitLoading.value = false
  }
}

function handleClose() {
  emit('close')
}
</script>
