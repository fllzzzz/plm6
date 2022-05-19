<template>
  <el-form ref="formRef" :model="form" :rules="rules" label-position="right" label-width="130px">
    <el-form-item label="科目" prop="classifyId">
      {{ props.technologyRow.classifyName }}
    </el-form-item>
    <el-form-item v-if="props.technologyRow.specification" label="规格" prop="material">
      {{ props.technologyRow.specification }}
    </el-form-item>
    <el-form-item v-if="props.technologyRow.color" label="颜色" prop="color">
      {{ props.technologyRow.color }}
    </el-form-item>
    <el-form-item v-if="props.technologyRow.brand" label="品牌" prop="brand">
      {{ props.technologyRow.brand }}
    </el-form-item>
    <el-form-item :label="`数量（${props.technologyRow.measureUnit}）`" prop="quantity">
      <common-input-number
        v-model="form.quantity"
        :min="0"
        :max="999999999"
        :step="1"
        :precision="props.technologyRow.measurePrecision"
        :controls="false"
        text-align="left"
        size="mini"
        :placeholder="`数量（${props.technologyRow.measureUnit}）`"
        style="width: 200px"
      />
    </el-form-item>
    <el-form-item :label="`核算量（${props.technologyRow.accountingUnit}）`" prop="mete">
      <common-input-number
        v-model="form.mete"
        :min="0"
        :max="999999999"
        :step="1"
        :precision="props.technologyRow.accountingPrecision"
        :controls="false"
        text-align="left"
        size="mini"
        :placeholder="`核算量（${props.technologyRow.accountingUnit}）`"
        style="width: 200px"
      />
    </el-form-item>
    <el-form-item label="备注" prop="remark">
      <el-input v-model.trim="form.remark" maxlength="250" size="mini" placeholder="备注" style="width: 200px" />
    </el-form-item>
  </el-form>
</template>

<script setup>
import { defineExpose, defineProps, ref } from 'vue'
import { positiveNumPattern } from '@/utils/validate/pattern'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import usePurchaseMaterialFormInfo from '../../composables/use-purchase-material-form-info'
import materialCascader from '@comp-cls/material-cascader/index.vue'

const props = defineProps({
  technologyRow: {
    type: Object,
    default: () => ({})
  },
  classifyIds: {
    type: Array,
    default: () => []
  }
})

// 基础分类
const basicClass = rawMatClsEnum.STEEL_PLATE.V

const rules = ref({
  classifyId: [{ required: true, message: '请选择科目', trigger: 'change' }],
  length: [
    { required: true, message: '请输入钢板长度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '钢板长度必须大于0', trigger: 'blur' }
  ],
  width: [
    { required: true, message: '请输入钢板宽度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '钢板宽度必须大于0', trigger: 'blur' }
  ],
  quantity: [{ pattern: positiveNumPattern, message: '数量必须大于0', trigger: 'blur' }]
})

// 物料基础分类单位配置
const { baseUnit } = useMatBaseUnit(rawMatClsEnum.STEEL_PLATE.V)
const { form, formRef, add, resetForm } = usePurchaseMaterialFormInfo({ props, basicClass })

defineExpose({
  add,
  resetForm
})
</script>
