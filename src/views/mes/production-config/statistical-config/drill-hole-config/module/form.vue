<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :show-close="false"
    custom-class="mes-cutting-config"
    width="30%"
    top="10vh"
  >
    <template #titleRight>
      <span style="float: right">
        <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">
          提 交
        </common-button>
        <common-button size="mini" @click="crud.cancelCU">关 闭</common-button>
      </span>
    </template>
    <div class="form">
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="170px" class="demo-form">
        <el-form-item label="零件规格前缀" prop="specPrefix">
          <common-select
            v-model="form.specPrefix"
            :options="[partKeyWordEnum.PL]"
            clearable
            type="enum"
            :disabled="isEdit"
            placeholder="零件规格前缀"
            style="width: 100%"
            class="input-underline"
          />
        </el-form-item>
        <el-form-item label="板厚数值（毫米）" prop="numThickness">
          <common-input-number
            v-model="form.minThickness"
            :step="1"
            :min="0"
            :precision="2"
            clearable
            :controls="false"
            size="mini"
            class="input-underline"
            placeholder="最小板厚(毫米)"
            style="width: 45%"
          />
          <span> ~ </span>
          <common-input-number
            v-model="form.maxThickness"
            :step="1"
            :precision="2"
            class="input-underline"
            :controls="false"
            size="mini"
            clearable
            placeholder="最大板厚(毫米)"
            style="width: 45%"
          />
        </el-form-item>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref, computed } from 'vue'
import { regForm } from '@compos/use-crud'
import { partKeyWordEnum } from '@enum-ms/mes'
// import { materialTypeEnum } from '@enum-ms/uploading-form'

const formRef = ref()

const defaultForm = {
  id: undefined
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

// 是否是编辑状态
const isEdit = computed(() => {
  return crud.status.edit > 0
})

const validateNumThickness = (rule, value, callback) => {
  if (!form.minThickness || !form.maxThickness) {
    callback(new Error('请填写板厚'))
  } else if (form.maxThickness < form.minThickness) {
    callback(new Error('最大板厚不得小于最小板厚'))
  }
  callback()
}
const rules = {
  specPrefix: [{ required: true, message: '请选择零件规格前缀', trigger: 'change' }],
  numThickness: [{ required: true, validator: validateNumThickness, message: '请填写板厚', trigger: 'blur' }]
}
</script>

<style lang="scss" scoped></style>
