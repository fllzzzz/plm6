<template>
  <div class="logistics-form">
    <el-divider><span class="title">物流信息</span></el-divider>
    <el-form :model="form.logistics" size="small" inline label-position="right" label-width="80px">
      <el-form-item label="运费" prop="freight" label-width="50px">
        <el-input-number
          class="input-underline"
          v-model="form.logistics.freight"
          type="text"
          :min="0"
          :max="999999999"
          :precision="2"
          :controls="false"
          placeholder="运费"
          style="width: 150px"
        />
      </el-form-item>
      <el-form-item label="装车费" prop="loadingFee">
        <el-input-number
          class="input-underline"
          v-model="form.logistics.entruckPrice"
          :min="0"
          :max="999999999"
          :precision="2"
          :controls="false"
          size="mini"
          placeholder="装车费"
          style="width: 150px"
        />
      </el-form-item>
      <el-form-item label="其他杂费" prop="otherFees">
        <el-input-number
          class="input-underline"
          v-model="form.logistics.entruckPrice"
          :min="0"
          :max="999999999"
          :precision="2"
          :controls="false"
          size="mini"
          placeholder="其他杂费"
          style="width: 150px"
        />
      </el-form-item>
      <el-form-item class="el-form-item-10" label="发票及税率" prop="invoiceType" label-width="100px">
        <invoice-type-select
          class="input-underline"
          v-model:invoiceType="form.logistics.invoiceType"
          v-model:taxRate="form.logistics.taxRate"
          :disabled="form.boolUsed"
          default
          :classification="supplierClassEnum.LOGISTICS.V"
        />
      </el-form-item>
      <el-form-item label="物流单位" prop="OtherFees">
        <supplier-select
          class="input-underline"
          v-model="form.logistics.supplierId"
          :type="supplierTypeEnum.LOGISTICS.V"
          placeholder="可选择供应商搜索"
          style="width: 250px"
          logistics-createable
          clearable
        />
      </el-form-item>
      <el-form-item>
        <span class="form-item-tip">* 也可在入库单审核通过后，于物流订单模块处添加</span>
      </el-form-item>
    </el-form>
  </div>
</template>

<script setup>
import { supplierTypeEnum, supplierClassEnum } from '@/utils/enum/modules/supplier'

import { regExtra } from '@/composables/form/use-form'
import supplierSelect from '@/components-system/base/supplier-select.vue'
import invoiceTypeSelect from '@/components-system/base/invoice-type-select.vue'
const { form } = regExtra() // 表单
</script>

<style lang="scss" scoped>
.logistics-form {
    width: 100%;
}
</style>
