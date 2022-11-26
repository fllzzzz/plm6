<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        class="filter-item"
        :productType="TechnologyTypeAllEnum.STRUCTURE.V"
        @getAreaInfo="getAreaInfo"
        :show-tips="areaInfo.length<=0"
      />
      <area-tabs
        class="filter-item"
        :style="areaInfo.length>0?'width:calc(100% - 230px)':'width:calc(100% - 380px)'"
        v-model="query.areaId"
        :area-info="areaInfo"
        :default-tab="defaultTab"
        @tab-click="tabClick"
      />
      <el-radio-group v-model="query.status" size="small" class="filter-item" @change="crud.toQuery">
        <el-radio-button :label="undefined">全部</el-radio-button>
        <el-radio-button v-for="item in processingEnum.ENUM" :key="item.V" :label="item.V">
          {{ item.L }}
        </el-radio-button>
      </el-radio-group>
      <el-input
        v-model="query.name"
        size="small"
        placeholder="输入名称搜索"
        style="width: 170px; margin-left: 0"
        class="filter-item"
        clearable
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
        <export-button v-permission="permission.download" :params="query" :fn="downLoad">
          构件清单（根据查询条件）
        </export-button>
      </template>
      <template #viewLeft>
        <el-tag v-loading="summaryData.loading" effect="plain" size="medium">
          构件清单量合计（数量/总净重/总毛重）：{{ summaryData.quantity }}件 / {{ summaryData.totalNetWeight }}kg / {{ summaryData.totalGrossWeight }}kg
        </el-tag>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { summary, downLoad } from '@/api/plan/technical-manage/artifact'
import { defineProps, ref, inject } from 'vue'

import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { processingEnum } from '@enum-ms/plan'
import checkPermission from '@/utils/system/check-permission'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { ElRadioGroup } from 'element-plus'
import monomerSelect from '@/components-system/plan/monomer-select'
import areaTabs from '@/components-system/plan/area-tabs'
import ExportButton from '@comp-common/export-button/index.vue'

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

const defaultQuery = {
  name: '',
  serialNumber: '',
  specification: '',
  material: '',
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  status: { value: undefined, resetAble: false }
}

const monomerSelectRef = ref()
const currentArea = ref({})
const areaInfo = ref([])
const defaultTab = ref({})
const summaryData = ref({
  loading: false,
  quantity: 0,
  totalNetWeight: 0,
  totalGrossWeight: 0
})

const permission = inject('permission')

const { crud, query, CRUD } = regHeader(defaultQuery)

CRUD.HOOK.handleRefresh = (crud, res) => {
  fetchSummary()
}

// 获取项目汇总数据
async function fetchSummary() {
  if (!checkPermission(permission.get)) return
  summaryData.value.loading = true
  try {
    const { quantity = 0, totalNetWeight = 0, totalGrossWeight = 0 } = await summary(query) || {}
    summaryData.value.quantity = quantity
    summaryData.value.totalNetWeight = totalNetWeight
    summaryData.value.totalGrossWeight = totalGrossWeight
  } catch (error) {
    console.log('获取构件清单汇总数据', error)
  } finally {
    summaryData.value.loading = false
  }
}

function tabClick(val) {
  const { name, label } = val
  currentArea.value = {
    id: name,
    name: label
  }
  crud.toQuery()
}

function getAreaInfo(val) {
  areaInfo.value = val || []
  if (areaInfo.value.length > 0) {
    defaultTab.value = {
      id: areaInfo.value[0].id + '',
      name: areaInfo.value[0].name
    }
  } else {
    defaultTab.value = {}
  }
}
</script>
