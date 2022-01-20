<template>
  <div class="head-container">
    <div v-if="!hiddenArea" v-show="crud.searchToggle">
      <monomer-select-area-tabs
        :productType="productType"
        :category="category"
        needConvert
        :project-id="projectId"
        @change="fetchMonomerAndArea"
      />
      <product-type-query :productType="productType" :category="category" :toQuery="crud.toQuery" :query="query" />
      <rrOperation />
    </div>
    <crudOperation>
      <template v-slot:optRight>
        <!-- 任务录入按钮 -->
        <template v-if="(query.areaId || hiddenArea) && checkPermission(permission.save)">
          <common-button v-show="modifying" type="warning" size="mini" @click.stop="handelModifying(false, true)">取消录入</common-button>
          <common-button v-show="modifying" type="success" size="mini" @click.stop="previewVisible = true">预览并保存</common-button>
          <common-button v-show="!modifying" type="primary" style="margin-left: 0px" size="mini" @click.stop="handelModifying(true)">
            任务录入
          </common-button>
          <el-popover
            v-if="(query.areaId || hiddenArea) && checkPermission(permission.clear)"
            v-model:visible="clearPopVisible"
            placement="right"
            width="600"
          >
            <p>确认清空【{{ currentArea.name }}】下的所有任务么？</p>
            <p>清空任务后，原有任务分配单作废，但已经进入生产流程的产品数量无法归零</p>
            <p>例子：</p>
            <p>【操作前】构件数量：50 | 已分配数量：40 | 未分配数量：10 | 进入生产流程数量：20</p>
            <p>【操作后】构件数量：50 | 已分配数量：20 | 未分配数量：30 | 进入生产流程数量：20</p>
            <div style="text-align: right; margin: 0">
              <common-button size="mini" type="text" @click="clearPopVisible = false">取消</common-button>
              <common-button type="primary" size="mini" @click="handleClear(crud.selections, productType)">确定</common-button>
            </div>
            <template #reference>
              <common-button :loading="clearLoading" size="mini" type="danger" :disabled="!crud.selections || !crud.selections.length">
                清空任务</common-button
              >
            </template>
          </el-popover>
        </template>
        <common-button
          v-if="(query.areaId || hiddenArea) && checkPermission(permission.save)"
          type="warning"
          size="mini"
          @click.stop="openQuicklyAssignDlg"
        >
          快速分配
        </common-button>
      </template>
      <template v-slot:viewLeft>
        <common-button :loading="!loaded" type="success" size="mini" @click.stop="productionLineVisible = true">{{
          !loaded ? '生产线加载中' : '选择生产线'
        }}</common-button>
      </template>
    </crudOperation>
  </div>
  <mPreview v-model:visible="previewVisible" :data="crud.data" :lines="lines" @success="handleSaveSuccess" />
  <production-line-drawer v-model:visible="productionLineVisible" :lines="lines" @changeLines="handleChangeLines" />
  <quickly-assign-drawer v-model:visible="quicklyAssignVisible" :data="crud.data" :lines="lines" @success="handleSaveSuccess" />
</template>

<script setup>
import { ref, defineProps, defineEmits, inject } from 'vue'
import checkPermission from '@/utils/system/check-permission'

import { deepClone } from '@data-type/index'

import useGetLines from '@compos/mes/scheduling/use-get-lines'
import useFormatSchedulingList from '@compos/mes/scheduling/use-format-scheduling-list'
import useSchedulingClear from '@compos/mes/scheduling/use-scheduling-clear'
import { regHeader } from '@compos/use-crud'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import { ElMessage } from 'element-plus'
import monomerSelectAreaTabs from '@comp-base/monomer-select-area-tabs'
import productTypeQuery from '@comp-mes/header-query/product-type-query'
import productionLineDrawer from '../production-line-drawer'
import quicklyAssignDrawer from '../quickly-assign-drawer'
import mPreview from '../scheduling-preview'

const defaultQuery = {
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  projectId: { value: undefined, resetAble: false }
}
const { crud, query, CRUD } = regHeader(defaultQuery)
const projectId = useGlobalProjectIdChangeToQuery(crud)
const permission = inject('permission')

const props = defineProps({
  modifying: {
    type: Boolean,
    default: false
  },
  hiddenArea: {
    type: Boolean,
    default: false
  },
  lines: {
    type: Array,
    default: () => []
  }
})

const emit = defineEmits(['update:lines', 'update:modifying', 'refreshSummary'])

const previewVisible = ref(false) // 分配预览dlg
const quicklyAssignVisible = ref(false) // 快速分配dlg
// TODO
const currentArea = {
  name: ''
}
const productType = inject('productType')
const category = inject('category', undefined)
const { productionLineVisible, loaded, lineLoad, schedulingMapTemplate } = useGetLines({ emit, dataHasFormatHook })
const { clearPopVisible, clearLoading, handleClear } = useSchedulingClear({ successHook: refresh })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.schedulingList = v.schedulingProductionLineDTOS || [] // 排产列表
    v.quantity = v.quantity || 0 // 清单数量
    v.sourceAssignQuantity = v.totalSchedulingQuantity || 0 // 已分配数量
    v.assignQuantity = v.sourceAssignQuantity || 0 // 已分配数量
    v.sourceUnassignQuantity = v.quantity - v.totalSchedulingQuantity // 未分配数量
    v.unassignQuantity = v.sourceUnassignQuantity // 未分配数量
    if (lineLoad.value) {
      // 生产线已加载，则进行数据格式转换
      v.schedulingMap = useFormatSchedulingList(v.schedulingList, schedulingMapTemplate)
      v.sourceSchedulingMap = deepClone(v.schedulingMap) // 用于数据还原和比较
    }
    return v
  })
}

function dataHasFormatHook() {
  if (loaded.value) {
    // 如果列表已经加载则对列表数据做一次处理
    crud.data = crud.data.map((v) => {
      // 排产表单：schedulingMap，对于页面任务分配的数量存储在schedulingMap中， k-v, k:productionLineId, v:表单内容
      v.schedulingMap = useFormatSchedulingList(v.schedulingList, schedulingMapTemplate)
      v.sourceSchedulingMap = deepClone(v.schedulingMap)
      return v
    })
  }
}

function openQuicklyAssignDlg() {
  if (props.modifying) {
    ElMessage({ message: '请先退出任务录入', type: 'warning' })
  } else {
    quicklyAssignVisible.value = true
  }
}

function handleSaveSuccess() {
  handelModifying(false)
  refresh()
  emit('refreshSummary')
}

function refresh() {
  crud.toQuery()
}

function handleChangeLines(changeLines) {
  crud.data.forEach((v) => {
    for (const id in changeLines) {
      // 取消选择还原数据
      if (!changeLines[id]) {
        const changeQuantity = (v.schedulingMap[id]?.quantity || 0) - (v.schedulingMap[id]?.sourceQuantity || 0)
        v.unassignQuantity += changeQuantity
        v.assignQuantity -= changeQuantity
      }
    }
  })
}

function handelModifying(modifying, reset = false) {
  // 取消分配，数据还原
  if (reset) {
    crud.data.forEach((v) => {
      v.schedulingMap = deepClone(v.sourceSchedulingMap)
      v.assignQuantity = v.sourceAssignQuantity // 已分配数量还原
      v.unassignQuantity = v.sourceUnassignQuantity // 未分配数量还原
      return v
    })
  }
  emit('update:modifying', modifying)
}

function fetchMonomerAndArea({ monomerId, areaId }) {
  query.monomerId = monomerId
  query.areaId = areaId
  crud.toQuery()
}
</script>
