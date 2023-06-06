<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        :productType="TechnologyTypeAllEnum.BRIDGE.V"
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
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        size="small"
        clearable
        placeholder="请选择区域"
        class="filter-item"
        style="width:200px;"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
      />
      <el-input
        v-model="query.specification"
        size="small"
        placeholder="输入规格搜索"
        style="width: 170px"
        class="filter-item"
        clearable
      />
      <el-input
        v-model="query.material"
        size="small"
        placeholder="输入材质搜索"
        style="width: 170px"
        class="filter-item"
        clearable
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #optLeft>
        <el-tag v-if="amount?.totalNetWeight">{{`总量:${amount?.totalNetWeight?.toFixed(DP.COM_WT__T)}t`}}</el-tag>
      </template>
      <template #viewLeft>
        <print-table
          v-permission="crud.permission.print"
          api-key="cellSummary"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref, watch } from 'vue'
import { regHeader } from '@compos/use-crud'
import { cellTotalWeight } from '@/api/bridge/bridge-plan/cell-summary'

import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { DP } from '@/settings/config'

import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'

const defaultQuery = {
  serialNumber: '',
  specification: '',
  material: '',
  monomerId: undefined,
  areaId: undefined,
  status: undefined
}

const monomerSelectRef = ref()
const areaInfo = ref([])
const amount = ref({})
const { crud, query } = regHeader(defaultQuery)
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

watch(
  () => query,
  (val) => {
    getTotalWeight()
  },
  { deep: true, immediate: true }
)

function getAreaInfo(val) {
  areaInfo.value = val || []
}

async function getTotalWeight() {
  amount.value = {}
  if (!query.projectId) {
    return
  }
  try {
    const data = await cellTotalWeight({ ...query })
    amount.value = data || {}
  } catch (e) {
    console.log('获取异常分段', e)
  }
}
</script>
