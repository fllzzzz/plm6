<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="550px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="130px">
      <el-form-item label="工厂" prop="factoryId">
        <factory-select :disabled="isEdit" v-model="form.factoryId" placeholder="请选择工厂" style="width: 270px" />
      </el-form-item>
      <el-form-item label="车间" prop="workshopId">
        <workshop-select
          :disabled="isEdit"
          v-model="form.workshopId"
          :factory-id="form.factoryId"
          placeholder="请先选择车间"
          style="width: 270px"
        />
      </el-form-item>
      <el-form-item label="生产线名称" prop="name">
        <el-input v-model="form.name" type="text" placeholder="请填写生产线名称" style="width: 270px" />
      </el-form-item>
      <el-form-item label="目标产量(吨/月)" prop="targetProductionShow">
        <el-input-number v-model.number="form.targetProductionShow" :min="0" :max="999999999" controls-position="right" style="width: 270px" />
      </el-form-item>
      <!-- <el-form-item label="生产线类型" prop="productionLineTypeEnum">
        <el-select v-model="form.productionLineTypeEnum" placeholder="请选择生产线类型" :size="'small'" style="width: 270px">
          <el-option v-for="item in artifactProductLineEnum.ENUM" :key="item.V" :label="item.L" :value="item.V" />
        </el-select>
      </el-form-item> -->
      <el-form-item label="产品类型" prop="productType">
        <common-select
          :dataStructure="{ key: 'K', label: 'L', value: 'V' }"
          v-model="form.productType"
          :options="componentTypeEnum.ENUM"
          placeholder="请选择产品类型"
          style="width: 270px"
        />
      </el-form-item>
      <el-form-item
        v-if="form.productType & componentTypeEnum.BOX.V"
        label="可生产产品种类"
        prop="linkIdList"
      >
        <common-select
          :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
          v-model="form.linkIdList"
          :options="configList"
          :loading="configLoading"
          multiple
          placeholder="请选择可生产产品种类"
          style="width: 270px"
        />
      </el-form-item>
      <el-form-item label="排序" prop="sort">
        <el-input-number v-model.number="form.sort" :min="1" :max="999" :step="1" controls-position="right" style="width: 270px" />
      </el-form-item>
      <el-form-item label="备注" prop="remark">
        <el-input
          v-model="form.remark"
          type="textarea"
          :autosize="{ minRows: 4, maxRows: 6 }"
          placeholder="请填写备注"
          style="width: 320px"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { productConfigInfo } from '@/api/bridge/production-config/production-line'
import { ref, computed, watch, watchEffect } from 'vue'

import { bridgeProcessTypeEnum as componentTypeEnum } from '@enum-ms/bridge'
import { artifactProductLineEnum } from '@enum-ms/mes'

import { regForm } from '@compos/use-crud'
import factorySelect from '@comp-base/factory-select.vue'
import workshopSelect from '@/components-system/bridge/workshop-select'

const formRef = ref()

const defaultForm = {
  id: undefined,
  factoryId: undefined,
  workshopId: undefined,
  productType: undefined,
  productionLineTypeEnum: artifactProductLineEnum.TRADITION.V,
  targetProductionShow: undefined,
  targetProduction: undefined,
  name: '',
  // shortName: '',
  sort: 1,
  remark: ''
}
const configList = ref([])
const configLoading = ref(false)

const { crud, form } = regForm(defaultForm, formRef)
const isEdit = computed(() => crud.status.edit >= 1)

const rules = {
  workshopId: [{ required: true, message: '请选择车间', trigger: 'change' }],
  factoryId: [{ required: true, message: '请选择工厂', trigger: 'change' }],
  productType: [{ required: true, message: '请选择产品类型', trigger: 'change' }],
  productionLineTypeEnum: [{ required: true, message: '请选择生产线类型', trigger: 'change' }],
  linkIdList: [{ required: true, message: '请选择', trigger: 'change' }],
  sort: [{ required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }],
  name: [
    { required: true, message: '请填写生产线名称', trigger: 'blur' },
    { min: 1, max: 32, message: '长度在 1 到 32 个字符', trigger: 'blur' }
  ],
  targetProductionShow: [{ required: true, message: '请填写目标产量', trigger: 'blur', type: 'number' }],
  // shortName: [
  //   { required: true, message: '请填写生产线简称', trigger: 'blur' },
  //   { min: 1, max: 32, message: '长度在 1 到 32 个字符', trigger: 'blur' }
  // ],
  remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }]
}

watchEffect(() => {
  form.targetProduction = form.targetProductionShow * 1000
})

watch(
  () => [form.productType],
  () => {
    if (form.productType) {
      fetchConfigInfo()
    }
  },
  { immediate: true }
)

async function fetchConfigInfo() {
  try {
    configLoading.value = true
    const { productionLineTypeEnum, productType } = form
    const content = await productConfigInfo({ productionLineTypeEnum, productType })
    configList.value = content
  } catch (error) {
    console.log('获取可生产类型配置信息', error)
  } finally {
    configLoading.value = false
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
