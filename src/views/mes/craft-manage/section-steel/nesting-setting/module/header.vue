<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.productionLineTypeEnum"
      :options="artifactProductLineEnum.ENUM"
      type="enum"
      default
      class="filter-item"
      @change="handleProductionLineTypeChange"
    />
    <common-radio-button
      v-if="!isTradition"
      v-model="query.boolMainAssemble"
      :options="assembleTypeEnum.ENUM"
      type="enum"
      class="filter-item"
      @change="fetchSummary"
    />
    <monomer-select-area-select
      v-model:monomerId="query.monomerId"
      v-model:areaId="query.areaId"
      clearable
      areaClearable
      :project-id="query.projectId"
    />
    <common-select
      v-model="query.specPrefix"
      :options="specPrefixList"
      type="other"
      :data-structure="{ key: 'specPrefix', label: 'specPrefix', value: 'specPrefix' }"
      class="filter-item"
      clearable
      style="width: 200px"
      placeholder="请选择截面类型"
      @change="handleSpecPreFixChange"
    />
    <common-select
      v-model="query.specification"
      :options="specificationList"
      type="other"
      clearable
      :data-structure="{ key: 'specification', label: 'specification', value: 'specification' }"
      class="filter-item"
      style="width: 200px"
      placeholder="请选择规格"
      @change="handleSpecificationChange"
    />
    <common-radio-button
      v-if="materialList.length"
      v-model="query.material"
      :options="materialList"
      show-option-all
      clearable
      type="other"
      :data-structure="{ key: 'material', label: 'material', value: 'material' }"
      class="filter-item"
      @change="fetchSummary"
    />
  </div>
  <div v-show="crud.searchToggle">
    <el-tag effect="plain" size="medium" class="filter-item">长度≥：</el-tag>
    <el-input type="number" v-model.trim="query.length" class="filter-item" size="small" placeholder="输入长度搜索" style="width: 117px" />
    <el-input
      v-model.trim="query.serialNumber"
      size="small"
      placeholder="输入编号搜索"
      style="width: 200px"
      class="filter-item"
      clearable
      @keyup.enter="fetchSummary"
    />
    <rrOperation />
  </div>
  <tag-tabs
    v-model="query.structureClassId"
    class="filter-item"
    style="width: 100%"
    :data="summaryList"
    itemKey="structureClassId"
    @change="crud.toQuery"
  >
    <template #default="{ item }">
      <span>{{ item.name }}：</span>
      <span>{{ item.totalQuantity }}件</span>
      <span> | </span>
      <span>{{ item.totalNetWeight }}吨</span>
    </template>
  </tag-tabs>
  <crudOperation>
    <template #optLeft>
      <common-radio-button
        v-if="isTradition"
        v-model="curMode"
        :options="[
          { type: 'nesting', label: '套料模式' },
          { type: 'edit', label: '编辑模式' },
        ]"
        :dataStructure="{ key: 'type', label: 'label', value: 'type' }"
        type="other"
        class="filter-item"
        @change="handleModeChange"
      />
      <common-button
        v-if="curMode === 'nesting'"
        class="filter-item"
        type="success"
        size="mini"
        icon="el-icon-menu"
        :disabled="!query.structureClassId || crud.selections.length === 0"
        @click="handleExtrusionNesting"
      >
        型材套排
      </common-button>
      <common-button
        v-if="curMode === 'edit'"
        class="filter-item"
        type="warning"
        size="mini"
        icon="el-icon-setting"
        @click="handleNotNeedNesting"
      >
        移入【无需套料清单】
      </common-button>
    </template>
    <template #viewLeft>
      <common-button v-if="isTradition" class="filter-item" type="success" size="mini" icon="el-icon-view" @click="noNestingVisible = true">
        查看【无需套料清单】
      </common-button>
    </template>
  </crudOperation>
  <!-- <filter-drawer v-model:visible="filterVisible" :list="filterList"></filter-drawer> -->
  <no-nesting-drawer v-model:visible="noNestingVisible" @refresh="fetchSummary" />
  <extrusion-nesting-setting v-model:visible="dialogVisible" :detail-data="crud.selections" :projectId="globalProjectId" />
</template>

<script setup>
import { getCondition, setNotNeedNesting, getNestingSummary } from '@/api/mes/craft-manage/section-steel/nesting-setting'
import { ref, watch, computed, watchEffect, defineEmits } from 'vue'
import { mapGetters } from '@/store/lib'
import { ElMessage, ElNotification, ElMessageBox } from 'element-plus'

import { artifactProductLineEnum, assembleTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import tagTabs from '@comp-common/tag-tabs'
import noNestingDrawer from './no-nesting-drawer.vue'
import extrusionNestingSetting from './extrusion-nesting-setting.vue'
import { deepClone } from '@/utils/data-type'

const emits = defineEmits(['change-mode'])

const { globalProjectId } = mapGetters('globalProjectId')
const defaultQuery = {
  projectId: globalProjectId.value,
  structureClassId: undefined,
  boolMainAssemble: assembleTypeEnum.MAIN_ASSEMBLE.V
}

const { crud, query } = regHeader(defaultQuery)

const noNestingVisible = ref(false)
const dialogVisible = ref(false)
const detailData = ref({})
const curMode = ref('nesting')
const specPrefixList = ref([])
const specificationList = ref([])
const materialList = ref([])
const summaryList = ref([])

const isTradition = computed(() => query.productionLineTypeEnum === artifactProductLineEnum.TRADITION.V)

watchEffect(() => {
  query.projectId = globalProjectId.value
})

watch(
  [() => query.projectId, () => query.monomerId, () => query.areaId],
  ([monomerId, areaId]) => {
    fetchOtherCondition()
  },
  { immediate: true }
)

function handleProductionLineTypeChange(val) {
  if (!isTradition.value) {
    curMode.value = 'nesting'
    handleModeChange(curMode.value)
  }
  fetchOtherCondition()
}

async function fetchOtherCondition() {
  if (!query.projectId) return
  try {
    specPrefixList.value = []
    const data = await getCondition({
      projectId: query.projectId,
      productionLineTypeEnum: query.productionLineTypeEnum,
      monomerId: query.monomerId,
      areaId: query.areaId
    })
    specPrefixList.value = data?.content || []
    fetchSummary()
  } catch (er) {
    console.log(er, '获取其他筛选条件列表')
  }
}

async function fetchSummary() {
  query.structureClassId = undefined
  if (!query.projectId) return
  try {
    summaryList.value = []
    const _query = deepClone(query)
    if (_query.productionLineTypeEnum === artifactProductLineEnum.TRADITION.V) {
      delete _query.boolMainAssemble
    }
    const data = await getNestingSummary(_query)
    summaryList.value = data?.content || []
    if (summaryList.value?.length) {
      query.structureClassId = summaryList.value[0].structureClassId
    }
    crud.toQuery()
  } catch (er) {
    console.log(er, '获取汇总列表')
  }
}

function handleSpecPreFixChange(val) {
  specificationList.value = specPrefixList.value.find((v) => v.specPrefix === val)?.specificationList || []
  fetchSummary()
}

function handleSpecificationChange(val) {
  materialList.value =
    specificationList.value
      .find((v) => v.specification === val)
      ?.material?.map((v) => {
        return {
          material: v
        }
      }) || []
  fetchSummary()
}

function handleModeChange(mode) {
  emits('change-mode', mode)
}

async function handleNotNeedNesting() {
  if (!crud.selections?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }

  try {
    ElMessageBox.confirm(`是否确认将所选的数据移入【无需套料清单】内`, '提示', {
      confirmButtonText: '确认',
      cancelButtonText: '取消',
      type: 'warning'
    }).then(async () => {
      try {
        const _data = crud.selections.map((v) => {
          return {
            // id: v.id,
            assembleDetailId: v.assembleDetailId,
            quantity: v.editQuantity
          }
        })
        await setNotNeedNesting(_data)
        ElNotification({
          title: '移入成功',
          type: 'success',
          duration: 2500
        })
        fetchSummary()
      } catch (error) {
        console.log('无需套料设置失败', error)
      }
    })
  } catch (er) {
    console.log(er, '无需套料设置失败')
  }
}

// 型材套排
function handleExtrusionNesting(val) {
  dialogVisible.value = true
  detailData.value = val
}
</script>
