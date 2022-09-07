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
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="170px" label-position="right">
      <el-form-item label="零件规格前缀" prop="specPrefix">
        <common-select
          v-model="form.specPrefix"
          :options="partsSpecPrefixEnum.ENUM"
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
import { ref, computed } from 'vue'
import { partsSpecPrefixEnum } from '@enum-ms/mes'
import { regForm } from '@compos/use-crud'

const formRef = ref()
const defaultForm = {
  id: undefined
}

const { crud, form } = regForm(defaultForm, formRef)

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

const validateNumBoreDiameter = (rule, value, callback) => {
  if (!form.minBoreDiameter || !form.maxBoreDiameter) {
    callback(new Error('请填写孔径'))
  } else if (form.maxBoreDiameter < form.minBoreDiameter) {
    callback(new Error('最大孔径不得小于最小孔径'))
  }
  callback()
}

const rules = {
  specPrefix: [{ required: true, message: '请选择零件规格前缀', trigger: 'change' }],
  unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  numThickness: [{ required: true, validator: validateNumThickness, message: '请填写板厚', trigger: 'blur' }],
  numBoreDiameter: [{ required: true, validator: validateNumBoreDiameter, message: '请填写孔径', trigger: 'blur' }]
}
</script>
