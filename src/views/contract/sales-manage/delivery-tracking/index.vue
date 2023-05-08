<template>
  <div class="app-container">
    <div class="head-container">
      <div>
        <div>
           <project-visa-select
            v-model="searchQuery.projectId"
            :default-id="globalProjectId"
            class="filter-item"
            style="width: 300px"
            placeholder="可选择项目搜索"
          />
          <common-radio-button
            v-model="productType"
            :options="packTypeEnum.ENUM"
            default
            :unshowVal="[packTypeEnum.MACHINE_PART.V]"
            type="enumSL"
            size="small"
            class="filter-item"
          />
          <el-date-picker
            v-model="searchQuery.date"
            type="daterange"
            range-separator=":"
            size="small"
            class="filter-item date-item"
            start-placeholder="开始时间"
            end-placeholder="结束时间"
            style="width: 240px"
          />
        </div>
        <div style="display:flex;" v-if="checkPermission(permission.get)">
          <div style="flex:1;padding:0 10px;padding-left:0;">
            <Panel name="累计发运量（吨）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.sumQuantity || 0" :precision="0" />
          </div>
          <div style="flex:1;padding:0 10px;">
            <Panel name="累计发运额（元）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.sumQuantity || 0" :precision="0" />
          </div>
          <div style="flex:1;padding:0 10px;">
            <Panel name="累计车次" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.sumQuantity || 0" :precision="0" />
          </div>
          <div style="flex:1;padding:0 10px;">
            <Panel name="筛选日期发运量（吨）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.sumQuantity || 0" :precision="0" />
          </div>
          <div style="flex:1;padding:0 10px;padding-right:0;">
            <Panel name="筛选日期发运额（元）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.sumQuantity || 0" :precision="0" />
          </div>
        </div>
      </div>
    </div>
    <component :is="currentView" ref="domRef" />
  </div>
</template>

<script setup>
// import { cost, priceModifyCount } from '@/api/contract/sales-manage/price-manage/common'
import { ref, computed, onMounted, provide } from 'vue'
import { mapGetters } from '@/store/lib'
import { deliveryTrackingPM as permission } from '@/page-permission/contract'

import { packTypeEnum } from '@enum-ms/mes'
// import { debounce } from '@/utils'
// import { isBlank } from '@data-type/index'
import checkPermission from '@/utils/system/check-permission'

import structure from './structure'
import enclosure from './enclosure'
import auxiliaryMaterial from './auxiliary-material'
import projectVisaSelect from '@comp-base/project-visa-select'
import Panel from '@/components/Panel'

// 当前显示组件
const currentView = computed(() => {
  switch (productType.value) {
    // case packTypeEnum.ENCLOSURE.V:
    //   return enclosure
    case packTypeEnum.AUXILIARY_MATERIAL.V:
      return auxiliaryMaterial
    default:
      return structure
  }
})

const { globalProjectId } = mapGetters(['globalProjectId'])

const domRef = ref()
const searchQuery = ref({})
const productType = ref()
const totalAmount = ref({})

provide('searchQuery', searchQuery)

onMounted(() => {
  handleProjectChange()
})

// 项目变动
function handleProjectChange() {
  // fetchCost()
  // fetchModifyCount()
}

// // 单体变动
// function handleMonomerChange() {
//   fetchCost()
// }

// 获取项目造价
// const fetchCost = debounce(async function () {
//   if (!checkPermission(permission.cost) || isBlank(projectId.value)) {
//     projectCost.value = 0
//     monomerCost.value = 0
//     return
//   }
//   try {
//     costLoading.value = true
//     const params = {
//       projectId: projectId.value,
//       monomerId: monomerId.value
//     }
//     const { monomerPrice, projectPrice } = await cost(params)
//     projectCost.value = projectPrice || 0
//     monomerCost.value = monomerPrice || 0
//   } catch (error) {
//     console.log(error)
//   } finally {
//     costLoading.value = false
//   }
// }, 100, false)

// 获取待审核数量
// const fetchModifyCount = debounce(async function () {
//   if (!checkPermission(permission.list)) return
//   try {
//     modifyCount.value = await priceModifyCount()
//   } catch (error) {
//     console.log('变更记录未审核数量', error)
//   }
// }, 100, false)

// 刷新数据
function refreshData() {
  // handleProjectChange()
  // domRef.value.refresh()
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
