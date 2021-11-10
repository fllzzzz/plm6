<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <!-- <monomerAreaTabs :project-id="projectId" @change="fetchMonomerAndArea" /> -->
      <slot name="customSearch" />
      <rrOperation />
    </div>
    <crudOperation>
      <template v-slot:optRight>
        <!-- 任务录入按钮 -->
        <template v-if="true || (query.districtId && useCheckPermission(permission.save))">
          <template v-if="modifying">
            <common-button type="warning" size="mini" @click.stop="handelModifying(false, true)">取消录入</common-button>
            <common-button type="success" size="mini" @click.stop="previewVisible = true">预览并保存</common-button>
          </template>
          <common-button v-else type="primary" size="mini" @click.stop="handelModifying(true)">任务录入</common-button>
          <el-popover
            v-if="true || (query.districtId && useCheckPermission(permission.clear))"
            v-model:visible="clearPopVisible"
            placement="top"
            width="600"
          >
            <p>确认清空【{{ currentArea.name }}】下的所有任务么？</p>
            <p>清空任务后，原有任务分配单作废，但已经进入生产流程的产品数量无法归零</p>
            <p>例子：</p>
            <p>【操作前】构件数量：50 | 已分配数量：40 | 未分配数量：10 | 进入生产流程数量：20</p>
            <p>【操作后】构件数量：50 | 已分配数量：20 | 未分配数量：30 | 进入生产流程数量：20</p>
            <div style="text-align: right; margin: 0">
              <common-button size="mini" type="text" @click="clearPopVisible = false">取消</common-button>
              <common-button type="primary" size="mini" @click="handleClear">确定</common-button>
            </div>
            <template #reference>
              <common-button :loading="clearLoading" size="mini" type="danger">清空任务</common-button>
            </template>
          </el-popover>
        </template>
        <common-button
          v-if="true || (query.districtId && useCheckPermission(permission.save))"
          type="warning"
          size="mini"
          @click.stop="openQuicklyAssignDlg"
          >快速分配</common-button
        >
      </template>
      <template v-slot:viewLeft>
        <common-button :loading="lineLoading" type="success" size="mini" @click.stop="productionLineVisible = true">{{
          lineLoading ? '生产线加载中' : '选择生产线'
        }}</common-button>
      </template>
    </crudOperation>
  </div>
  <mPreview v-model:visible="previewVisible" :data="crud.data" :lines="lines" @success="handleSaveSuccess" />
  <production-line-drawer v-model:visible="productionLineVisible" :lines="lines" />
  <quickly-assign-drawer v-model:visible="quicklyAssignVisible" :data="crud.data" :lines="lines" @success="handleSaveSuccess" />
</template>

<script setup>
import { ref, defineProps, defineEmits, inject } from 'vue'
import { ElMessage } from 'element-plus'

import useCheckPermission from '@compos/use-check-permission'
import useGetLines from '@compos/mes/scheduling/use-get-lines'
import useFormatSchedulingList from '@compos/mes/scheduling/use-format-scheduling-list'
import useSchedulingClear from '@compos/mes/scheduling/use-scheduling-clear'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import productionLineDrawer from '../production-line-drawer'
import quicklyAssignDrawer from '../quickly-assign-drawer'
import mPreview from '../scheduling-preview'

const defaultQuery = {
  serialNumber: '',
  monomerId: { value: undefined, resetAble: false },
  districtId: { value: undefined, resetAble: false }
}
const { crud, query, CRUD } = regHeader(defaultQuery)
const permission = inject('permission')

const props = defineProps({
  modifying: {
    type: Boolean,
    default: false
  },
  projectId: {
    type: [Number, String],
    required: true
  },
  lines: {
    type: Array,
    default: () => []
  }
})

const emit = defineEmits(['update:lines', 'update:modifying'])

const { productionLineVisible, lineLoading, lineLoad, schedulingMapTemplate } = useGetLines({ emit, dataHasFormatHook })
const { clearPopVisible, clearLoading, handleClear } = useSchedulingClear({ successHook: refresh })

const previewVisible = ref(false) // 分配预览dlg
const quicklyAssignVisible = ref(false) // 快速分配dlg
const dataHasFormat = ref(false) // 排产数据格式是否已转换，未转换则在生产线加载成功时转换
// TODO
const currentArea = {
  name: ''
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  dataHasFormat.value = lineLoad.value // 数据格式是否已经转换，因为接口异步，所以dataHasFormat放在循环前赋值
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
      v.sourceSchedulingMap = JSON.parse(JSON.stringify(v.schedulingMap)) // 用于数据还原和比较
    }
    return v
  })
}

function dataHasFormatHook() {
  if (dataHasFormat.value) {
    // 如果列表已经加载则对列表数据做一次处理
    crud.data = crud.data.map((v) => {
      // 排产表单：schedulingMap，对于页面任务分配的数量存储在schedulingMap中， k-v, k:productionLineId, v:表单内容
      v.schedulingMap = useFormatSchedulingList(v.schedulingList, schedulingMapTemplate)
      v.sourceSchedulingMap = JSON.parse(JSON.stringify(v.schedulingMap))
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
}

function refresh() {
  crud.toQuery()
}

function handelModifying(modifying, reset = false) {
  // 取消分配，数据还原
  if (reset) {
    crud.data.forEach((v) => {
      v.schedulingMap = JSON.parse(JSON.stringify(v.sourceSchedulingMap))
      v.assignQuantity = v.sourceAssignQuantity // 已分配数量还原
      v.unassignQuantity = v.sourceUnassignQuantity // 未分配数量还原
      return v
    })
  }
  emit('update:modifying', modifying)
}

// function fetchMonomerAndArea({ monomerId, districtId }) {
//   query.monomerId = monomerId
//   query.districtId = districtId
//   crud.toQuery()
// }
</script>
