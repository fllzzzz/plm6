<template>
  <common-drawer ref="drawerRef" :title="`构件排产预览`" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="100%">
    <template #titleAfter>
      <el-tag size="small">
        <span>当前区域：</span>
        <span>{{ curAreaNames }}</span>
      </el-tag>
    </template>
    <template #titleRight>
      <common-button v-show="selectionMode === selectionModeEnum.SCHEDULING.V" size="mini" type="success" @click="toAssembleScheduling">
        下一步【部件排产】
      </common-button>
    </template>
    <template #content>
      <div class="head-container">
        <common-radio-button
          v-model="queryVO.productionLineTypeEnum"
          :options="artifactProductLineEnum.ENUM"
          type="enum"
          size="small"
          class="filter-item"
        />
        <tag-tabs
          ref="tagTabsRef"
          v-model="queryVO.structureClassId"
          class="filter-item"
          :style="'width:calc(100% - 150px)'"
          :data="artifactTypeList"
          :unselectable="artifactTypeList.length > 1"
          itemKey="structureClassId"
          @change="fetch"
        >
          <template #default="{ item }">
            <span>{{ item.name }}：</span>
            <span>{{ item.quantity }}件</span>
          </template>
        </tag-tabs>
        <el-input
          v-model.trim="queryVO.serialNumber"
          size="small"
          placeholder="输入编号搜索"
          style="width: 170px"
          class="filter-item"
          clearable
          @keyup.enter="fetch"
        />
        <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="fetch">搜索</common-button>
        <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
          重置
        </common-button>
        <common-radio-button
          v-model="selectionMode"
          size="mini"
          :options="selectionModeEnum"
          type="enum"
          class="filter-item"
          @change="handleModeChange"
        />
        <common-button v-if="selectionMode === selectionModeEnum.EDIT.V" size="mini" type="danger" style="float: right" @click="toBatchDel">
          批量删除
        </common-button>
        <el-tooltip class="item" effect="light" :disabled="!!queryVO.structureClassId" content="请选择构件类型" placement="left">
          <span style="float: right; margin-right: 5px">
            <common-button
              v-if="selectionMode === selectionModeEnum.EDIT.V"
              :disabled="!queryVO.structureClassId"
              size="mini"
              type="primary"
              @click="toBatchEdit"
            >
              批量重新分配
            </common-button>
          </span>
        </el-tooltip>
      </div>
      <common-table
        ref="recordTableRef"
        v-loading="tableLoading"
        :data="tableData"
        :span-method="spanMethod"
        :max-height="maxHeight"
        :stripe="false"
        :data-format="dataFormat"
        row-key="id"
        style="width: 100%"
        @selection-change="selectionChangeHandler"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="groups.name" :show-overflow-tooltip="true" label="车间>生产线>生产组" min-width="170" align="center">
          <template #default="{ row, $index }">
            <div class="flex-rsc">
              <el-checkbox
                v-model="row.groupCheck"
                :indeterminate="row.isIndeterminateCheck"
                @change="handleCheckAllChange($event, row, $index)"
              />
              <div class="flex-ccc" style="flex: 1">
                <span>{{ row.workshop?.name }}>{{ row.productionLine?.name }}>{{ row.groups?.name }}</span>
                <el-tag type="success" effect="plain" style="margin-top: 5px; padding: 0px 15px">
                  <span>{{ row.mergeQuantity }} 件</span>
                  <span style="margin-right: 5px; margin-left: 5px">|</span>
                  <span>{{ row.mergeWeight }} kg</span>
                </el-tag>
              </div>
            </div>
          </template>
        </el-table-column>
        <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体" min-width="100" align="center">
          <template #default="{ row }">
            <span>{{ row.monomer?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="area.name" :show-overflow-tooltip="true" label="区域" min-width="100" align="center">
          <template #default="{ row }">
            <span>{{ row.area?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="100" align="center">
          <template #default="{ row }">
            <span>{{ row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column type="selection" width="55" align="center" class="selection" :selectable="selectable" />
        <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="120" align="center" />
        <el-table-column prop="length" :show-overflow-tooltip="true" label="长度（mm）" min-width="90" align="center" />
        <el-table-column prop="schedulingQuantity" :show-overflow-tooltip="true" label="数量" min-width="90" align="center" />
        <el-table-column prop="schedulingTotalNetWeight" :show-overflow-tooltip="true" label="总重（kg）" min-width="90" align="center" />
        <el-table-column prop="askCompleteTime" :show-overflow-tooltip="true" label="完成日期" min-width="90" align="center" />
        <el-table-column v-if="selectionMode === selectionModeEnum.EDIT.V" label="操作" width="200" align="center">
          <template #default="{ row }">
            <common-button size="mini" type="primary" @click="toEdit(row)">重新分配</common-button>
            <common-button size="mini" type="danger" @click="toDel(row)">删除任务</common-button>
          </template>
        </el-table-column>
      </common-table>
      <edit-form v-model:visible="editVisible" :itemInfo="itemInfo" @refresh="fetch" />
      <batch-edit-form
        v-model:visible="batchEditVisible"
        :selections="selections"
        :structureClassId="queryVO.structureClassId"
        @refresh="fetch"
      />
      <del-form v-model:visible="delVisible" :itemInfo="itemInfo" @del-success="handleDelSuccess" />
      <batch-del-form v-model:visible="batchDelVisible" :selections="selections" @del-success="handleDelSuccess" />
      <assemble-scheduling-form
        v-model:visible="assembleVisible"
        :artifact-list="selections"
        :productionLineTypeEnum="listProductionLineTypeEnum"
        @task-issue-success="handleTaskIssueSuccess"
      />
    </template>
  </common-drawer>
</template>

<script setup>
import { record, getArtifactRecordType } from '@/api/mes/scheduling-manage/artifact'
import { ElMessage } from 'element-plus'
import { defineProps, defineEmits, ref, inject, computed, watch } from 'vue'

import { artifactProductLineEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useGetArtifactTypeList from '@compos/mes/scheduling/use-get-artifact-type-list'
import editForm from './edit-form'
import batchEditForm from './batch-edit-form'
import delForm from './del-form'
import batchDelForm from './batch-del-form'
import assembleSchedulingForm from './assemble-scheduling-form'
import tagTabs from '@comp-common/tag-tabs'

const drawerRef = ref()
const recordTableRef = ref()
const tagTabsRef = ref()
const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  otherQuery: {
    type: Object,
    default: () => {}
  }
})

const crud = inject('crud')

const selectionModeEnum = {
  SCHEDULING: { K: 'SCHEDULING', L: '排产模式', V: 1 },
  EDIT: { K: 'EDIT', L: '编辑模式', V: 2 }
}
const selectionMode = ref(selectionModeEnum.SCHEDULING.V)

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook, closeHook })

const { artifactTypeList, refreshArtifactType } = useGetArtifactTypeList({ getApi: getArtifactRecordType, initHook: artifactTypeInit }, true)

const areaIdObj = inject('areaIdObj')
const dataFormat = ref([['askCompleteTime', ['parse-time', '{y}-{m}-{d}']]])
const curAreaNames = computed(() => {
  return props.otherQuery?.areaIdList?.map((v) => areaIdObj.value[v].name).join('、') || ''
})

const tableData = ref([])
const tableLoading = ref(false)
const listObjIdsByGroup = ref({})
const queryVO = ref({
  productionLineTypeEnum: crud.query.productionLineTypeEnum || artifactProductLineEnum.TRADITION.V
})
const closeRefreshOut = ref(false)

const artifactTypeParams = computed(() => {
  return {
    areaIdList: props.otherQuery.areaIdList,
    productionLineTypeEnum: queryVO.value.productionLineTypeEnum
  }
})

const listProductionLineTypeEnum = computed(() => {
  return tagTabsRef.value?.getOption(queryVO.value.structureClassId)?.productionLineTypeEnum || queryVO.value.productionLineTypeEnum
})

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

watch(
  [() => queryVO.value.productionLineTypeEnum],
  () => {
    refreshArtifactType({ ...artifactTypeParams.value })
  },
  { deep: true }
)

function artifactTypeInit() {
  if (artifactTypeList.value?.length === 1) {
    queryVO.value.structureClassId = artifactTypeList.value[0].structureClassId
    fetch()
  } else {
    tableData.value = []
    listObjIdsByGroup.value = {}
  }
}

function handleModeChange() {
  recordTableRef.value?.clearSelection()
}

function showHook() {
  recordTableRef.value?.clearSelection()
  resetQuery()
  refreshArtifactType({ ...artifactTypeParams.value })
}

function closeHook() {
  // 有删除操作需刷新外层数据
  if (closeRefreshOut.value) {
    emit('refresh')
  }
}

function resetQuery() {
  queryVO.value.serialNumber = undefined
  queryVO.value.structureClassId = undefined
}

async function fetch() {
  try {
    tableLoading.value = true
    tableData.value = []
    listObjIdsByGroup.value = {}
    const { content } = await record({ ...props.otherQuery, ...queryVO.value })
    const _list = content
    let _curNeedMergeIndex = 0 // 首行为初始需要的合并行
    let _mergeRowspan = 0
    let _mergeQuantity = 0
    let _mergeWeight = 0
    for (let i = 0; i < _list.length; i++) {
      const _curGroupId = _list[i].groups?.id
      _list[i].mergeIndex = _curNeedMergeIndex
      // 处理首行
      if (i === 0) {
        _mergeRowspan++
        _mergeQuantity += _list[i].schedulingQuantity
        _mergeWeight += _list[i].schedulingTotalNetWeight
      }
      if (i > 0) {
        // 与上行合并
        if (_curGroupId === _list[i - 1].groups?.id) {
          _mergeRowspan++
          _mergeQuantity += _list[i].schedulingQuantity
          _mergeWeight += _list[i].schedulingTotalNetWeight
          _list[_curNeedMergeIndex].rowspan = 0
        } else {
          // 不合并
          // 赋值已合计的信息给上一个合并行
          _list[_curNeedMergeIndex].rowspan = _mergeRowspan
          _list[_curNeedMergeIndex].mergeQuantity = _mergeQuantity
          _list[_curNeedMergeIndex].mergeWeight = _mergeWeight.toFixed(2)
          // 重置合并信息
          _mergeRowspan = 1
          _mergeQuantity = _list[i].schedulingQuantity
          _mergeWeight = _list[i].schedulingTotalNetWeight
          // 赋值当前的index
          _curNeedMergeIndex = i
        }
      }
      // 最后一行时 处理最后的_curNeedMergeIndex
      if (i === _list.length - 1) {
        _list[_curNeedMergeIndex].rowspan = _mergeRowspan
        _list[_curNeedMergeIndex].mergeQuantity = _mergeQuantity
        _list[_curNeedMergeIndex].mergeWeight = _mergeWeight.toFixed(2)
      }
      // 其他处理数据
      _list[i].needSchedulingQuantity = _list[i].schedulingQuantity
      _list[i].groupCheck = false
      _list[i].isIndeterminateCheck = false
      if (!listObjIdsByGroup.value[_curGroupId]) {
        listObjIdsByGroup.value[_curGroupId] = []
      }
      listObjIdsByGroup.value[_curGroupId].push(_list[i].id)
    }
    tableData.value = _list
  } catch (error) {
    console.log('获取构件排产预览记录失败', error)
  } finally {
    tableLoading.value = false
  }
}

function handleCheckAllChange(val, row, index) {
  const _groupId = row.groups?.id
  console.log(row, _groupId, val, listObjIdsByGroup.value[_groupId], 'handleCheckAllChange')
  if (_groupId && listObjIdsByGroup.value[_groupId]) {
    tableData.value.forEach((v) => {
      if (listObjIdsByGroup.value[_groupId].includes(v.id)) {
        recordTableRef?.value?.toggleRowSelection(v, val)
      }
    })
  }
  if (!val) {
    tableData.value[index].isIndeterminateCheck = false
  }
}

// 合并单元格
function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (columnIndex === 1) {
    return {
      rowspan: row.rowspan || 0,
      colspan: 1
    }
  }
}

// 不同生产组的禁止选择
function selectable(row, rowIndex) {
  if (selectionMode.value === selectionModeEnum.EDIT.V || listProductionLineTypeEnum.value === artifactProductLineEnum.INTELLECT.V) {
    if (!selections.value?.length) {
      return true
    } else {
      return row.groups?.id === selections.value[0]?.groups?.id
    }
  }
  return true
}

// --------------------------- 操作数据 start ------------------------------

const itemInfo = ref({})
const selections = ref([])

function selectionChangeHandler(val) {
  console.log(val, 'selectionChangeHandler')
  if (val.length) {
    for (const item in listObjIdsByGroup.value) {
      const _groupsId = Number(item)
      const compareLength = listObjIdsByGroup.value[item].length
      const _list = val.filter((v) => v.groups.id === _groupsId)
      console.log(_groupsId, compareLength, _list.length, 'compareLength')
      if (_list.length) {
        const _index = _list[0].mergeIndex
        tableData.value[_index].isIndeterminateCheck = _list.length > 0 && _list.length < compareLength
        tableData.value[_index].groupCheck = _list.length > 0 && _list.length === compareLength
      }
    }
  } else {
    for (let i = 0; i < tableData.value.length; i++) {
      const v = tableData.value[i]
      v.isIndeterminateCheck = false
      v.groupCheck = false
    }
  }

  selections.value = val
}
// --------------------------- 操作数据 end --------------------------------

// --------------------------- 重新分配 start ------------------------------

const editVisible = ref(false)
const batchEditVisible = ref(false)

function toEdit(row) {
  itemInfo.value = Object.assign({}, row)
  editVisible.value = true
}

function toBatchEdit() {
  if (!selections.value?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  batchEditVisible.value = true
}

// --------------------------- 重新分配 end --------------------------------

// --------------------------- 删除任务 start ------------------------------

const delVisible = ref(false)
const batchDelVisible = ref(false)

function handleDelSuccess() {
  fetch()
  refreshArtifactType({ ...artifactTypeParams.value })
  closeRefreshOut.value = true // 删除会导致外层数据变更需刷新
}

function toDel(row) {
  itemInfo.value = Object.assign({}, row)
  delVisible.value = true
}

function toBatchDel() {
  if (!selections.value?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  batchDelVisible.value = true
}

// --------------------------- 删除任务 end --------------------------------

// --------------------------- 部件排产 start ------------------------------

const assembleVisible = ref(false)

function toAssembleScheduling() {
  if (!selections.value?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  assembleVisible.value = true
}

// --------------------------- 部件排产 end --------------------------------

function handleTaskIssueSuccess() {
  fetch()
  refreshArtifactType({ ...artifactTypeParams.value })
}
</script>
