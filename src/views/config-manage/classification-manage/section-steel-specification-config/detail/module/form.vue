<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :visible="crud.status.cu > CRUD.STATUS.NORMAL"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    :show-close="true"
    custom-class="section-steel-detail-form"
    width="400px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">
        确认
      </common-button>
    </template>
    <el-form
      ref="formRef"
      :model="form"
      :rules="rules"
      :disabled="crud.status.cu === CRUD.STATUS.PROCESSING"
      label-position="top"
      size="small"
      label-width="55px"
    >
      <el-form-item label="规格" prop="specification">
        <el-input v-model.trim="form.specification" type="text" placeholder="规格" style="width: 350px" />
      </el-form-item>
      <template v-for="sd in standard" :key="`${prefix}${sd.id}`">
        <el-form-item :label="`${sd.name} 理论重量(kg/m)`" :prop="`${prefix}${sd.id}`" label-width="100px">
          <el-input-number
            class="align-left"
            v-model="form[`${prefix}${sd.id}`]"
            type="text"
            :placeholder="`${sd.name} 理论重量(kg/m)`"
            controls-position="right"
            style="width: 350px"
            :min="0.001"
          />
        </el-form-item>
      </template>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, inject } from 'vue'
import { regForm } from '@compos/use-crud'

const defaultForm = {
  specification: '' // 规格
}

// 校验规则
const rules = ref({
  specification: [
    { required: true, message: '请填写规格', trigger: 'blur' },
    { max: 50, message: '不能超过50个字符', trigger: 'blur' }
  ]
})

const prefix = inject('prefix')
const standard = inject('standard') // 国标
const sectionSteel = inject('sectionSteel') // 当前选择的型材

// 表单设置国标默认值
standard.value.forEach((sd) => {
  defaultForm[`${prefix}${sd.id}`] = undefined
})

const formRef = ref()
const { CRUD, crud, form } = regForm(defaultForm, formRef)

// 表单提交数据清理
crud.submitFormFormat = (form) => {
  // 型材id
  form.sectionSteelId = sectionSteel.value.id

  form.standard = []
  standard.value.forEach((sd) => {
    const unitNet = form[`${prefix}${sd.id}`]
    if (unitNet) {
      form.standard.push({
        id: sd.id, // 国标id
        unitNet: form[`${prefix}${sd.id}`] // 单位净量
      })
    }
    delete form[`${prefix}${sd.id}`]
  })
  return form
}
</script>

<style lang="scss" scoped>
.section-steel-detail-form {
  .align-left {
    ::v-deep(.el-input__inner) {
      text-align: left;
    }
  }
}
</style>
