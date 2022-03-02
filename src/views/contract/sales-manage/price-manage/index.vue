<template>
  <div class="app-container">
    <div class="head-container">
      <div class="filter-container">
        <div class="filter-left-box">
          <common-radio-button
            v-model="productType"
            :options="packTypeEnum.ENUM"
            default
            type="enumSL"
            size="small"
            class="filter-item"
          />
          <monomer-select
            v-model="monomerId"
            :project-id="globalProjectId"
            class="filter-item"
            @change="handleMonomerChange"
          />
        </div>
        <div class="filter-right-box">
          <template v-if="checkPermission(permission.cost)">
            <el-tag effect="plain" size="medium" class="filter-item">
              项目造价：
              <span v-if="!costLoading" v-thousand="projectCost" />
              <i v-else class="el-icon-loading" />
            </el-tag>
            <el-tag v-if="monomerId" effect="plain" size="medium" class="filter-item">
              单体造价：
              <span v-if="!costLoading" v-thousand="monomerCost" />
              <i v-else class="el-icon-loading" />
            </el-tag>
          </template>
          <el-badge v-if="checkPermission(permission.get)" :value="modifyCount" :hidden="modifyCount <= 0">
            <common-button class="filter-item" size="mini" type="info" @click="modifyVisible = true">变更记录</common-button>
          </el-badge>
        </div>
      </div>
    </div>
    <component :is="currentView" ref="domRef" @refresh-count="fetchModifyCount" />
    <!-- 商务变更记录 -->
    <common-drawer
      append-to-body
      :close-on-click-modal="false"
      :visible="modifyVisible"
      title="变更记录"
      size="80%"
      :before-close="
        () => {
          modifyVisible = false
        }
      "
    >
      <template #content>
        <modifyRecord  @refresh-data="refreshData" />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import { cost, priceModifyCount } from '@/api/contract/sales-manage/price-manage/common'
import { ref, computed, watch, onMounted, provide } from 'vue'
import { mapGetters } from '@/store/lib'
import { priceManagePM as permission } from '@/page-permission/contract'

import { packTypeEnum } from '@enum-ms/mes'
import { debounce } from '@/utils'
import checkPermission from '@/utils/system/check-permission'

import monomerSelect from '@/components-system/plan/monomer-select'
import structure from './structure'
import enclosure from './enclosure'
import auxiliaryMaterial from './auxiliary-material'
import modifyRecord from './price-modify-list/index'

// 当前显示组件
const currentView = computed(() => {
  switch (productType.value) {
    case packTypeEnum.ENCLOSURE.V:
      return enclosure
    case packTypeEnum.AUXILIARY_MATERIAL.V:
      return auxiliaryMaterial
    default:
      return structure
  }
})

const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])

// 围护结算类型
const enclosureMeasureMode = ref()

watch(
  globalProjectId,
  () => {
    handleProjectChange()
  }
)

watch(
  globalProject,
  () => {
    enclosureMeasureMode.value = globalProject.value?.enclosureMeasureMode
  },
  { immediate: true }
)

const domRef = ref()
const productType = ref()
const modifyCount = ref(0)
const costLoading = ref(false)
const projectCost = ref(0)
const monomerCost = ref(0)
const monomerId = ref()
const modifyVisible = ref(false)

provide('monomerId', monomerId)
provide('projectId', globalProjectId)
provide('modifyVisible', modifyVisible)
provide('enclosureMeasureMode', enclosureMeasureMode)

onMounted(() => {
  handleProjectChange()
})

// 项目变动
function handleProjectChange() {
  fetchCost()
  fetchModifyCount()
}

// 单体变动
function handleMonomerChange() {
  fetchCost()
}

// 获取项目造价
const fetchCost = debounce(async function () {
  if (!checkPermission(permission.cost)) return
  try {
    costLoading.value = true
    const params = {
      projectId: globalProjectId.value,
      monomerId: monomerId.value
    }
    const { monomerPrice, projectPrice } = await cost(params)
    projectCost.value = projectPrice || 0
    monomerCost.value = monomerPrice || 0
  } catch (error) {
    console.log(error)
  } finally {
    costLoading.value = false
  }
}, 100, false)

// 获取待审核数量
const fetchModifyCount = debounce(async function () {
  if (!checkPermission(permission.list)) return
  try {
    modifyCount.value = await priceModifyCount()
  } catch (error) {
    console.log('变更记录未审核数量', error)
  }
}, 100, false)

// 刷新数据
function refreshData() {
  handleProjectChange()
  domRef.value.refresh()
}
</script>

<style lang="scss" scoped>
$default-cell-mask-color: #52f09840;
::v-deep(.mask-td) {
  .cell {
    &:after {
      background-color: $default-cell-mask-color;
    }
  }
}
::v-deep(.el-table.businessTable) {
  th,
  td {
    padding: 0;
  }
  .cell {
    line-height: 46px;
    padding:0;
  }
  th .cell {
    padding: 0 10px;
  }
  td:first-child .cell {
    padding: 0;
  }
  .el-table__body .el-input__inner,
  .el-table__body .el-textarea__inner {
    border-radius: 0;
  }

  .cell {
    .el-input__inner {
      border: none;
    }
    .el-input-number__increase {
      border-left: none;
      margin-right: 10px;
    }
    .el-input-number__decrease {
      border-right: none;
      margin-left: 10px;
    }
  }
}
</style>
