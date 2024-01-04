<template>
  <div class="logistics-form">
    <el-divider><span class="title">物流信息</span></el-divider>
    <el-form
      ref="formRef"
      :model="currentForm"
      :disabled="props.disabled"
      :rules="rules"
      size="small"
      inline
      label-position="right"
      label-width="80px"
    >
      <el-form-item label="运费" prop="freight" label-width="60px">
        <common-input-number
          class="input-underline"
          v-model="currentForm.freight"
          type="text"
          :min="0"
          :max="999999999"
          :precision="DP.YUAN"
          :controls="false"
          placeholder="运费"
          style="width: 150px"
        />
      </el-form-item>
      <el-form-item label="运输重量（kg）" prop="freightWeight" label-width="120px">
        <common-input-number
          class="input-underline"
          v-model="currentForm.freightWeight"
          type="text"
          :min="0"
          :max="999999999"
          :precision="DP.COM_WT__KG"
          :controls="false"
          placeholder="运输重量"
          style="width: 150px"
        />
      </el-form-item>
      <!-- <el-form-item label="装车费" prop="loadingFee">
        <common-input-number
          class="input-underline"
          v-model="currentForm.entruckPrice"
          :min="0"
          :max="999999999"
          :precision="DP.YUAN"
          :controls="false"
          size="mini"
          placeholder="装车费"
          style="width: 150px"
        />
      </el-form-item>
      <el-form-item label="其他杂费" prop="otherFees">
        <common-input-number
          class="input-underline"
          v-model="currentForm.entruckPrice"
          :min="0"
          :max="999999999"
          :precision="DP.YUAN"
          :controls="false"
          size="mini"
          placeholder="其他杂费"
          style="width: 150px"
        />
      </el-form-item> -->
      <el-form-item class="el-form-item-10" label="发票及税率" prop="invoiceType" label-width="100px">
        <invoice-type-select
          class="input-underline"
          v-model:invoiceType="currentForm.invoiceType"
          v-model:taxRate="currentForm.taxRate"
          :disabled="currentForm.boolUsed"
          default
          :classification="supplierClassEnum.LOGISTICS.V"
        />
      </el-form-item>
      <el-form-item label="物流单位" prop="supplierId">
        <supplier-select
          class="input-underline"
          v-model="currentForm.supplierId"
          :type="supplierTypeEnum.LOGISTICS.V"
          placeholder="可选择供应商搜索"
          style="width: 250px"
          logistics-createable
          :show-type="false"
          clearable
        />
      </el-form-item>
      <!-- <el-form-item>
        <span class="form-item-tip">* 也可在入库单审核通过后，于物流订单模块处添加</span>
      </el-form-item> -->
    </el-form>
  </div>
</template>

<script setup>
import { ref, defineProps, defineExpose, watchEffect } from 'vue'
import { supplierTypeEnum, supplierClassEnum } from '@/utils/enum/modules/supplier'
import { DP } from '@/settings/config'

import supplierSelect from '@/components-system/base/supplier-select/index.vue'
import invoiceTypeSelect from '@/components-system/base/invoice-type-select.vue'
import { isNotBlank, isBlank } from '@/utils/data-type'

const props = defineProps({
  disabled: {
    type: Boolean,
    default: false
  },
  form: {
    type: Object,
    default: () => {
      return {}
    }
  }
})

const formRef = ref()
const currentForm = ref({})

watchEffect(() => {
  currentForm.value = props.form
})

const validateFreight = (rule, value, callback) => {
  if (isNotBlank(currentForm.value.supplierId) && isBlank(currentForm.value.freight)) {
    callback(new Error('请输入运费'))
  }
  callback()
}

const validateFreightWeight = (rule, value, callback) => {
  if (isNotBlank(currentForm.value.freightWeight)) {
    if (!currentForm.value.freightWeight) {
      callback(new Error('运输重量必须大于0'))
    }
  }
  callback()
}

const validateSupplierId = (rule, value, callback) => {
  if (isNotBlank(currentForm.value.freight) && isBlank(currentForm.value.supplierId)) {
    callback(new Error('请选择供应商'))
  }
  callback()
}

const rules = {
  freight: [
    { required: true, message: '请输入运费', trigger: 'blur' },
    { validator: validateFreight, trigger: 'blur' }
  ],
  freightWeight: [
    { validator: validateFreightWeight, trigger: 'blur' }
  ],
  supplierId: [{ validator: validateSupplierId, trigger: 'change' }]
}

// // 表单提交前校验
// FORM.HOOK.beforeSubmit = async () => {
//   const res = await validate()
//   return res
// }

// 表单校验
async function validate() {
  try {
    if (formRef.value) {
      const res = await formRef.value.validate()
      return res
    } else {
      return false
    }
  } catch (error) {
    return false
  }
}

defineExpose({
  validate
})
</script>

<style lang="scss" scoped>
// form {
//     width: 100%;
// }
</style>
