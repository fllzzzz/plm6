<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.productType"
        :options="deliveryInstallTypeEnum.ENUM"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="query.projectId"
        :main-product-type="query.productType"
        :default="false"
        clearable
        class="filter-item"
        @change="crud.toQuery"
        @getAreaInfo="getAreaInfo"
      />
       <common-select
        v-model="query.areaId"
        :options="areaInfo"
        type="other"
        :dataStructure="typeProp"
        size="small"
        clearable
        placeholder="请选择区域"
        class="filter-item"
        style="width:200px;"
        @change="crud.toQuery"
      />
      <el-input
        v-model.trim="query.serialNumber"
        placeholder="编号搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.name"
        placeholder="名称搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #viewLeft>
        <common-button type="primary" size="mini" :disabled="crud.data.length===0" @click="openConfirm" v-if="crud.permission.add && globalProject?.installReportMethod === installSetEnum.PC.V">提交预览</common-button>
      </template>
    </crudOperation>
    <detail-confirm v-model="confirmVisible" :submitList="submitList" :projectId="crud.query.projectId" :productType="crud.query.productType" @success="crud.toQuery"/>
  </div>
</template>

<script setup>
import { ref, defineProps, watch } from 'vue'

import { deliveryInstallTypeEnum, installSetEnum } from '@enum-ms/project'
// import checkPermission from '@/utils/system/check-permission'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import detailConfirm from './detail-confirm'
import { ElMessage } from 'element-plus'

const defaultQuery = {
  projectId: { value: undefined, resetAble: false },
  productType: deliveryInstallTypeEnum.ARTIFACT.V,
  monomerId: undefined,
  areaId: undefined,
  name: undefined,
  serialNumber: undefined
}
const { crud, query } = regHeader(defaultQuery)
const typeProp = { key: 'id', label: 'name', value: 'id' }
const areaInfo = ref([])
const confirmVisible = ref(false)
const submitList = ref([])

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  globalProject: {
    type: Object,
    default: () => {}
  }
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function getAreaInfo(val) {
  areaInfo.value = val || []
}

function openConfirm() {
  const filterData = crud.data.filter(v => v.reportQuantity && v.reportQuantity > 0)
  submitList.value = []
  if (filterData.length > 0) {
    confirmVisible.value = true
    submitList.value = filterData
  } else {
    ElMessage.error('请填写本次安装填报数量且大于0')
  }
}
</script>
