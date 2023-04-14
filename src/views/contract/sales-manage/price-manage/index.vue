<template>
  <div class="app-container">
    <div class="head-container">
      <div class="filter-container">
        <div class="filter-left-box">
          <project-visa-select
            v-model="projectId"
            :default-id="globalProjectId"
            class="filter-item"
            style="width: 300px"
            placeholder="可选择项目搜索"
          />
          <common-radio-button
            v-model="productType"
            :options="contractSaleTypeEnum.ENUM"
            default
            type="enumSL"
            size="small"
            class="filter-item"
          />
          <!-- <monomer-select
            v-model="monomerId"
            :project-id="projectId"
            class="filter-item"
            @change="handleMonomerChange"
          /> -->
        </div>
        <div class="filter-right-box">
          <template v-if="checkPermission(permission.cost) && productType!==contractSaleTypeEnum.AUXILIARY_MATERIAL.V">
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
          <el-badge v-if="checkPermission(permission.list)" :value="modifyCount" :hidden="modifyCount <= 0">
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
import { ref, computed, onMounted, provide } from 'vue'
import { mapGetters } from '@/store/lib'
import { priceManagePM as permission } from '@/page-permission/contract'

import { contractSaleTypeEnum } from '@enum-ms/mes'
import { debounce } from '@/utils'
import { isBlank } from '@data-type/index'
import checkPermission from '@/utils/system/check-permission'

// import monomerSelect from '@/components-system/plan/monomer-select'
import structure from './structure'
// import enclosure from './enclosure'
import auxiliaryMaterial from './auxiliary-material'
import machinePart from './machine-part'
import modifyRecord from './price-modify-list/index'
import projectVisaSelect from '@comp-base/project-visa-select'

// 当前显示组件
const currentView = computed(() => {
  switch (productType.value) {
    // case contractSaleTypeEnum.ENCLOSURE.V:
    //   return enclosure
    case contractSaleTypeEnum.MACHINE_PART.V:
      return machinePart
    case contractSaleTypeEnum.AUXILIARY_MATERIAL.V:
      return auxiliaryMaterial
    default:
      return structure
  }
})

const { globalProjectId } = mapGetters(['globalProjectId'])

const domRef = ref()
const projectId = ref()
const productType = ref()
const modifyCount = ref(0)
const costLoading = ref(false)
const projectCost = ref(0)
const monomerCost = ref(0)
const monomerId = ref()
const modifyVisible = ref(false)

// provide('monomerId', monomerId)
provide('projectId', projectId)
provide('modifyVisible', modifyVisible)

onMounted(() => {
  handleProjectChange()
})

// 项目变动
function handleProjectChange() {
  fetchCost()
  fetchModifyCount()
}

// 单体变动
// function handleMonomerChange() {
//   fetchCost()
// }

// 获取项目造价
const fetchCost = debounce(async function () {
  if (!checkPermission(permission.cost) || isBlank(projectId.value)) {
    projectCost.value = 0
    monomerCost.value = 0
    return
  }
  try {
    costLoading.value = true
    const params = {
      projectId: projectId.value
      // monomerId: monomerId.value
    }
    const { monomerPrice, projectPrice } = await cost(params)
    projectCost.value = projectPrice || 0
    // monomerCost.value = monomerPrice || 0
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

::v-deep(.wrong-td) {
  .cell {
    &:after {
      content: '';
      position: absolute;
      z-index: 1;
      background-color: #ff000021;
      top: 0;
      right: 0;
      bottom: 0;
      left: 0;
      pointer-events: none; // 穿透
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
