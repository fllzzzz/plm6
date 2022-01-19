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
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px">
      <el-form-item label="字典标签" prop="label">
        <el-input
          v-model="form.label"
          placeholder="输入字典标签"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
      </el-form-item>
      <el-form-item label="展宽(mm)" prop="dictionaryDetail.unfoldedWidth" v-if="(props.line.type===TechnologyTypeEnum.PROFILED_PLATE.V || line.type===TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V) && line.name==='plate_type'">
        <el-input-number
          class="align-left"
          v-model="form.dictionaryDetail.unfoldedWidth"
          placeholder="请填写"
          type="text"
          controls-position="right"
          style="width: 200px"
          :min="0"
          :max="999999999"
        />
      </el-form-item>
      <el-form-item label="有效宽度(mm)" prop="dictionaryDetail.effectiveWidth" v-if="(props.line.type===TechnologyTypeEnum.PROFILED_PLATE.V || line.type===TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V) && line.name==='plate_type'">
        <el-input-number
          class="align-left"
          v-model="form.dictionaryDetail.effectiveWidth"
          placeholder="请填写"
          type="text"
          controls-position="right"
          style="width: 200px"
          :min="0"
          :max="999999999"
        />
      </el-form-item>
      <el-form-item label="排序" prop="sort">
        <el-input-number
          class="align-left"
          v-model="form.sort"
          placeholder="请填写"
          type="text"
          controls-position="right"
          style="width: 200px"
          :min="0"
          :max="1000"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps } from 'vue'
import { regForm } from '@compos/use-crud'
import { TechnologyTypeEnum } from '@enum-ms/contract'

const formRef = ref()

const defaultForm = {
  id: undefined,
  label: undefined,
  sort: undefined,
  name: '',
  type: undefined,
  dictionaryDetail: {
    id: undefined,
    effectiveWidth: undefined,
    unfoldedWidth: undefined
  }
}

const { crud, form } = regForm(defaultForm, formRef)

const props = defineProps({
  line: {
    type: Object,
    default: () => {}
  }
})

const validateLabel = (rule, value, callback) => {
  const regInt = /^[+]{0,1}(\d+)$/ // 整数
  const regDP3 = /^(?!(0[0-9]{0,}$))[0-9]{1,}[.]{0,}[0-9]{0,3}$/ // 数字,3位小数
  const regDP2 = /^(?!(0[0-9]{0,}$))[0-9]{1,}[.]{0,}[0-9]{0,2}$/ // 数字,2位小数
  if (!value) {
    callback(new Error('请填写字典标签'))
  }
  if (['brand', 'mode', 'plating', 'yield_strength', 'plate_type', 'in_plating', 'out_plating', 'in_Steel_Plate_brand', 'out_steel_plate_brand', 'brand_core', 'out_plate_shape', 'in_plate_shape'].includes(props.line.name) && value.length > 20) {
    callback(new Error('长度在 20 个字符以内'))
  }
  if (['model', 'out_material', 'in_material', 'in_coating', 'out_coating', 'out_colour', 'in_colour', 'kind_core'].includes(props.line.name) && value.length > 10) {
    callback(new Error('长度在 10 个字符以内'))
  }
  if (['effective_width', 'out_effective_width', 'in_effective_width'].includes(props.line.name) && !regInt.test(value)) {
    callback(new Error(`请输入整数`))
  }
  if (['thickness', 'out_thickness', 'in_thickness'].includes(props.line.name) && !regDP3.test(value)) {
    callback(new Error(`请输入数字且最多保留3位小数`))
  }
  if (['unit_weight_core'].includes(props.line.name) && !regDP2.test(value)) {
    callback(new Error(`请输入数字且最多保留2位小数`))
  }
  callback()
}
const rules = {
  label: [
    { validator: validateLabel, trigger: 'blur' }
  ],
  'dictionaryDetail.effectiveWidth': [{ required: true, message: '请输入有效宽度', trigger: 'blur' }],
  'dictionaryDetail.unfoldedWidth': [{ required: true, message: '请输入展宽', trigger: 'blur' }],
  sort: [{ required: true, message: '请输入排序', trigger: 'blur', type: 'number' }]
}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
