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
          placeholder="请先选择工厂"
          style="width: 270px"
        />
      </el-form-item>
      <el-form-item label="生产线名称" prop="name">
        <common-select
          :disabled="isEdit"
          v-model="form.name"
          :options="productionLineList"
          type="other"
          placeholder="请选择或填写生产线"
          :data-structure="{ key: 'name', label: 'name', value: 'name' }"
          class="filter-item"
          filterable
          allow-create
          clearable
          style="width: 270px"
        />
      </el-form-item>
      <el-form-item label="目标产量(吨/月)" prop="targetProductionShow">
        <el-input-number
          v-model.number="form.targetProductionShow"
          :min="0"
          :max="999999999"
          controls-position="right"
          style="width: 270px"
        />
      </el-form-item>
      <el-form-item label="可生产产品种类" prop="linkIdList">
        <common-select
          v-model="form.linkIdList"
          :options="TechnologyTypeAllEnum.ENUM"
          :unshow-options="[TechnologyTypeAllEnum.STRUCTURE.K, TechnologyTypeAllEnum.BRIDGE.K]"
          multiple
          type="enum"
          size="small"
          placeholder="请选择可生产产品种类"
          style="width: 270px"
          @change="handleEnclosureType"
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
import { getProductionLineName } from '@/api/config/enclosure/production-config/production-line'
import { ref, computed, onMounted, watchEffect } from 'vue'

import { TechnologyTypeAllEnum } from '@enum-ms/contract'

import { regForm } from '@compos/use-crud'
import factorySelect from '@comp-base/factory-select.vue'
import workshopSelect from '@comp-mes/workshop-select'

const formRef = ref()

const defaultForm = {
  id: undefined,
  type: undefined, // 围护类型 （位运算）
  factoryId: undefined,
  workshopId: undefined,
  targetProductionShow: undefined,
  targetProduction: undefined,
  name: undefined,
  sort: 1,
  remark: ''
}

const productionLineList = ref([])

const { crud, CRUD, form } = regForm(defaultForm, formRef)
const isEdit = computed(() => crud.status.edit >= 1)

const rules = {
  workshopId: [{ required: true, message: '请选择车间', trigger: 'change' }],
  factoryId: [{ required: true, message: '请选择工厂', trigger: 'change' }],
  linkIdList: [{ required: true, message: '请选择可生产产品种类', trigger: 'change' }],
  sort: [{ required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }],
  name: [
    { required: true, message: '请填写或选择生产线名称', trigger: 'blur' },
    { min: 1, max: 32, message: '长度在 1 到 32 个字符', trigger: 'blur' }
  ],
  targetProductionShow: [{ required: true, message: '请填写目标产量', trigger: 'blur', type: 'number' }],
  remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }]
}

watchEffect(() => {
  form.targetProduction = form.targetProductionShow * 1000
})

onMounted(() => {
  fetchProductionLine()
})

async function fetchProductionLine() {
  productionLineList.value = []
  try {
    const data = await getProductionLineName({})
    data?.forEach((v) => {
      productionLineList.value.push({
        name: v
      })
    })
  } catch (error) {
    console.log('获取所有生产线失败', error)
  }
}

// 围护类型选择
function handleEnclosureType(val) {
  let type
  if (val) {
    val.forEach(v => {
      type |= v
    })
  }
  form.type = type
}

CRUD.HOOK.beforeSubmit = async () => {}

CRUD.HOOK.afterSubmit = async () => {
  fetchProductionLine()
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
