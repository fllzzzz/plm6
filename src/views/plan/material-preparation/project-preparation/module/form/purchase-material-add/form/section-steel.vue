<template>
  <el-form ref="formRef" :model="form" :rules="rules" label-position="right" label-width="80px">
    <el-form-item label="科目" prop="classifyId">
      <material-cascader
        check-strictly
        v-model="form.classifyId"
        :classifyId="classifyIds"
        default
        separator=" > "
        show-all-levels
        size="mini"
        class="filter-item"
        style="width: 200px"
        placeholder="可选择/输入科目、编号搜索"
      />
    </el-form-item>
    <el-form-item label="材质" prop="material">
      {{ props.technologyRow.material }}
    </el-form-item>
    <el-form-item label="规格" prop="material">
      {{ props.technologyRow.specification }}
    </el-form-item>
    <el-form-item :label="`长(${baseUnit.length.unit})`" prop="length">
      <common-input-number
        v-model="form.length"
        :min="0"
        :max="999999"
        :precision="baseUnit.length.precision"
        :controls="false"
        text-align="left"
        size="mini"
        placeholder="长"
        style="width: 200px"
      />
    </el-form-item>
    <el-form-item label="品牌" prop="brand">
      <el-input v-model.trim="form.brand" maxlength="60" size="mini" placeholder="品牌" style="width: 200px" />
    </el-form-item>
    <el-form-item label="数量" prop="quantity">
      <common-input-number
        v-model="form.quantity"
        :min="0"
        :max="999999999"
        :step="1"
        :precision="baseUnit.measure.precision"
        :controls="false"
        text-align="left"
        size="mini"
        placeholder="数量"
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
const basicClass = rawMatClsEnum.SECTION_STEEL.V

const rules = ref({
  classifyId: [{ required: true, message: '请选择科目', trigger: 'change' }],
  length: [
    { required: true, message: '请输入钢板长度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '钢板长度必须大于0', trigger: 'blur' }
  ],
  quantity: [{ pattern: positiveNumPattern, message: '数量必须大于0', trigger: 'blur' }]
})

// 物料基础分类单位配置
const { baseUnit } = useMatBaseUnit(basicClass)

const { form, formRef, add, resetForm } = usePurchaseMaterialFormInfo({ props, basicClass })

defineExpose({
  add,
  resetForm
})
</script>
