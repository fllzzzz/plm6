<template>
  <common-drawer ref="drawerRef" :title="`构件排产预览`" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="100%">
    <template #titleAfter>
      <el-tag size="small">
        <span>当前单体：</span>
        <span>{{ props.monomerDetail ? props.monomerDetail?.monomer?.name : tableData[0]?.monomer?.name }}</span>
      </el-tag>
      <el-tag size="small">
        <span>当前区域：</span>
        <span>{{ curAreaNames }}</span>
      </el-tag>
    </template>
    <template #titleRight>
      <common-button
        v-permission="permission.assembleGet"
        v-show="selectionMode === selectionModeEnum.SCHEDULING.V"
        size="mini"
        type="success"
        :disabled="tableData.length < 1"
        @click="toAssembleScheduling"
      >
        下一步【部件排产】
      </common-button>
    </template>
    <template #content>
      <div class="head-container">
        <!-- <group-header
          v-show="queryVO.structureClassId"
          v-model="queryVO.groupsId"
          :data="groupData"
          @task-issue-success="handleTaskIssueSuccess"
        /> -->
        <group-header v-model="queryVO.groupsId" :data="groupData" @task-issue-success="handleTaskIssueSuccess" />
        <common-radio-button
          v-if="lineTypeLoad && unshowLineType.length !== artifactProductLineEnum.KEYS.length"
          v-model="queryVO.productionLineTypeEnum"
          :options="hasIntelligent ? artifactProductLineEnum.ENUM : traditionLineEnum.ENUM"
          type="enum"
          size="small"
          :unshowVal="unshowLineType"
          default
          class="filter-item"
          @change="fetch"
        />
        <tag-tabs
          v-if="artifactTypeList.length"
          ref="tagTabsRef"
          v-model="queryVO.structureClassId"
          class="filter-item"
          :hit="selectionMode === selectionModeEnum.EDIT.V ? true : false"
          :unselectable="selectionMode === selectionModeEnum.EDIT.V ? false : true"
          :style="'width:calc(100% - 150px)'"
          style="display: inline-block"
          :data="artifactTypeList"
          itemKey="structureClassId"
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
        <span class="filter-item">
          <el-tag size="medium" effect="plain" style="margin-left: 5px"> 数量(件)：{{ summaryInfo.quantity || 0 }} </el-tag>
          <el-tag size="medium" effect="plain" style="margin-left: 5px">
            重量(kg)：{{ summaryInfo.totalNetWeight?.toFixed(2) || 0 }}
          </el-tag>
        </span>
        <common-button
          v-permission="permission.recordDel"
          v-if="selectionMode === selectionModeEnum.EDIT.V"
          size="mini"
          type="danger"
          style="float: right"
          @click="toBatchDel"
        >
          批量删除
        </common-button>
        <el-tooltip class="item" effect="light" :disabled="!!queryVO.structureClassId" content="请选择构件类型" placement="left">
          <span style="float: right; margin-right: 5px">
            <!-- <common-button
              v-permission="permission.recordEdit"
              v-if="selectionMode === selectionModeEnum.EDIT.V"
              :disabled="!queryVO.structureClassId"
              size="mini"
              type="primary"
              @click="toBatchEdit"
            >
              批量重新分配
            </common-button> -->
            <common-button
              v-permission="permission.recordEdit"
              v-if="selectionMode === selectionModeEnum.EDIT.V"
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
        :max-height="maxHeight - 50"
        :stripe="false"
        :data-format="dataFormat"
        row-key="id"
        :class="listProductionLineTypeEnum === artifactProductLineEnum.INTELLECT.V ? 'hidden-table-check-all' : ''"
        style="width: 100%"
        @selection-change="selectionChangeHandler"
      >
        <el-table-column
          v-if="selectionMode === selectionModeEnum.EDIT.V"
          type="selection"
          width="55"
          align="center"
          class="selection"
          :selectable="selectable"
        />
        <el-table-column label="序号" type="index" align="center" width="60" />
        <!-- <el-table-column prop="groups.name" :show-overflow-tooltip="true" label="车间>生产线>生产组" min-width="170" align="center">
          <template #default="{ row, $index }">
            <div class="flex-rsc">
              <el-checkbox
                v-model="row.groupCheck"
                :indeterminate="row.isIndeterminateCheck"
                :disabled="getDisabled(row)"
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
        </el-table-column> -->
        <!-- <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体" min-width="100" align="center">
          <template #default="{ row }">
            <span>{{ row.monomer?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="area.name" :show-overflow-tooltip="true" label="区域" min-width="100" align="center">
          <template #default="{ row }">
            <span>{{ row.area?.name }}</span>
          </template>
        </el-table-column> -->
        <el-table-column prop="structureClass" :show-overflow-tooltip="true" label="类型" min-width="100" align="center">
          <template #default="{ row }">
            <el-tag v-if="row.structureClass?.name">{{ row.structureClass?.name }}</el-tag>
          </template>
        </el-table-column>
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="100" align="center">
          <template #default="{ row }">
            <span>{{ row.serialNumber }}</span>
            <!-- <br />
            <el-tag v-if="row.structureClass?.name">{{ row.structureClass?.name }}</el-tag> -->
          </template>
        </el-table-column>
        <el-table-column prop="name" :show-overflow-tooltip="true" label="构件名称" min-width="100" align="center" />
        <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="120" align="center" />
        <el-table-column prop="length" :show-overflow-tooltip="true" label="长度（mm）" min-width="90" align="center" />
        <el-table-column prop="schedulingQuantity" :show-overflow-tooltip="true" label="数量" min-width="90" align="center" />
        <el-table-column prop="schedulingTotalNetWeight" :show-overflow-tooltip="true" label="总重（kg）" min-width="90" align="center" />
        <el-table-column prop="askCompleteTime" :show-overflow-tooltip="true" label="完成日期" min-width="90" align="center" />
        <el-table-column
          v-permission="[...permission.recordEdit, ...permission.recordDel]"
          v-if="selectionMode === selectionModeEnum.EDIT.V"
          label="操作"
          width="200"
          align="center"
        >
          <template #default="{ row }">
            <common-button v-permission="permission.recordEdit" size="mini" type="primary" @click="toEdit(row)">重新分配</common-button>
            <common-button v-permission="permission.recordDel" size="mini" type="danger" @click="toDel(row)">删除任务</common-button>
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <!-- <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      /> -->
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
        :artifact-list="tableData"
        :productionLineTypeEnum="listProductionLineTypeEnum"
        @task-issue-success="handleTaskIssueSuccess"
      />
    </template>
  </common-drawer>
</template>

<script setup>
import { record, getArtifactRecordType, getLineRecordType, recordSummary, groupSummary } from '@/api/mes/scheduling-manage/artifact'
import { ElMessage } from 'element-plus'
import { defineProps, defineEmits, ref, inject, computed, watch } from 'vue'
// import { getLightColor } from '@/utils/color'
import { artifactProductLineEnum, traditionLineEnum } from '@enum-ms/mes'
import { artifactSchedulingPM as permission } from '@/page-permission/mes'
import { mapGetters } from '@/store/lib'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
// import usePagination from '@compos/use-pagination'
import useGetArtifactTypeList from '@compos/mes/scheduling/use-get-artifact-type-list'
import editForm from './edit-form'
import batchEditForm from './batch-edit-form'
import delForm from './del-form'
import batchDelForm from './batch-del-form'
import assembleSchedulingForm from './assemble-scheduling-form'
import tagTabs from '@comp-common/tag-tabs'

import groupHeader from '@/views/mes/scheduling-manage/common/group-header.vue'

// const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetch })

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
  },
  monomerDetail: {
    type: Object,
    default: () => {}
  }
})

const { hasIntelligent } = mapGetters('hasIntelligent')

const selectionModeEnum = {
  SCHEDULING: { K: 'SCHEDULING', L: '排产模式', V: 1 },
  EDIT: { K: 'EDIT', L: '编辑模式', V: 2 }
}
const selectionMode = ref(selectionModeEnum.SCHEDULING.V)

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook, closeHook })

const { artifactTypeList, refreshArtifactType } = useGetArtifactTypeList(
  { getApi: getArtifactRecordType, initHook: artifactTypeInit },
  true
)

const areaIdObj = inject('areaIdObj')
const dataFormat = ref([['askCompleteTime', ['parse-time', '{y}-{m}-{d}']]])
const curAreaNames = computed(() => {
  return props.otherQuery?.areaIdList?.map((v) => areaIdObj.value[v].name).join('、') || ''
})

const tableData = ref([])
const tableLoading = ref(false)
const listObjIdsByGroup = ref({})
const summaryInfo = ref({})
const queryVO = ref({})
const closeRefreshOut = ref(false)
const unshowLineType = ref([])
const lineTypeLoad = ref(false)
const groupData = ref([])
// const colorObj = ref({}) // 班组id: color

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
    extraBox: ['.el-drawer__header', '.head-container'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    paginate: true,
    clientHRepMainH: true,
    extraHeight: 50
  },
  drawerRef
)

watch(
  [() => queryVO.value.productionLineTypeEnum],
  () => {
    refreshArtifactType({ ...artifactTypeParams.value })
    // fetchGroup()
  },
  { deep: true }
)
watch(
  [() => queryVO.value.structureClassId],
  (val) => {
    fetchGroup()
    fetch()
  },
  { deep: true }
)
watch(
  [() => queryVO.value.groupsId],
  (val) => {
    if (val) {
      fetch()
    }
  },
  { deep: true }
)

// 获取生产组信息
async function fetchGroup() {
  const areaIdList = props.otherQuery.areaIdList
  // if (!queryVO.value.structureClassId) return
  try {
    const { content } = (await groupSummary({ areaIdList, structureClassId: queryVO.value.structureClassId })) || {}
    // content.forEach((v) => {
    //   if (!colorObj.value[v.groups?.id]) {
    //     colorObj.value[v.groups?.id] = getLightColor()
    //   }
    //   v.groupsColor = colorObj.value[v.groups?.id]
    // })
    groupData.value = content || []
    if (content?.length) {
      queryVO.value.groupsId = content[0]?.groups?.id
    }
  } catch (error) {
    console.log('获取生产组信息失败', error)
  }
}

async function fetchLineType() {
  const areaIdList = props.otherQuery.areaIdList
  queryVO.value.productionLineTypeEnum = undefined
  unshowLineType.value = []
  lineTypeLoad.value = false
  tableData.value = []
  listObjIdsByGroup.value = {}
  summaryInfo.value = {}
  if (!areaIdList?.length) return
  try {
    const { content } = await getLineRecordType({ areaIdList })
    for (const item in artifactProductLineEnum.ENUM) {
      if (content.indexOf(artifactProductLineEnum[item].V) === -1) {
        unshowLineType.value.push(artifactProductLineEnum[item].V)
      }
    }
  } catch (er) {
    console.log('获取产线类型失败')
  } finally {
    lineTypeLoad.value = true
  }
}

function artifactTypeInit() {
  if (
    !artifactTypeList.value?.length ||
    (queryVO.value.structureClassId &&
      artifactTypeList.value?.length &&
      artifactTypeList.value.findIndex((v) => v.structureClassId === queryVO.value.structureClassId) === -1)
  ) {
    queryVO.value.structureClassId = undefined
  }
  if (
    artifactTypeList.value?.length &&
    (artifactTypeList.value?.length > 0 || queryVO.value.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V)
  ) {
    // queryVO.value.structureClassId = artifactTypeList.value[0].structureClassId
    queryVO.value.structureClassId = selectionMode.value === selectionModeEnum.EDIT.V ? artifactTypeList.value[0]?.structureClassId : undefined
  }
  fetch()
}

function handleModeChange() {
  recordTableRef.value?.clearSelection()
  queryVO.value.structureClassId = selectionMode.value === selectionModeEnum.EDIT.V ? artifactTypeList.value[0]?.structureClassId : undefined
}

function showHook() {
  fetchGroup()
  fetchLineType()
  recordTableRef.value?.clearSelection()
  resetQuery()
}

function closeHook() {
  // 有删除操作需刷新外层数据
  if (closeRefreshOut.value) {
    emit('refresh')
  }
}

function resetQuery() {
  queryVO.value.serialNumber = undefined
  queryVO.value.groupsId = groupData.value[0]?.groups?.id
  queryVO.value.structureClassId = selectionMode.value === selectionModeEnum.EDIT.V ? artifactTypeList.value[0]?.structureClassId : undefined
  fetch()
}

async function fetch() {
  tableData.value = []
  listObjIdsByGroup.value = {}
  summaryInfo.value = {}
  // if (!queryVO.value.structureClassId || !queryVO.value.productionLineTypeEnum) {
  //   return
  // }
  if (!queryVO.value.productionLineTypeEnum) {
    return
  }
  try {
    tableLoading.value = true
    summaryInfo.value = (await recordSummary({ ...props.otherQuery, ...queryVO.value })) || {}
    const content = (await record({ ...props.otherQuery, ...queryVO.value })) || []
    // const { content = [], totalElements } = await record({ ...props.otherQuery, ...queryVO.value, ...queryPage })
    // setTotalPage(totalElements)
    const _list = content
    let _curNeedMergeIndex = 0 // 首行为初始需要的合并行
    let _mergeRowspan = 0
    let _mergeQuantity = 0
    let _mergeWeight = 0
    for (let i = 0; i < _list.length; i++) {
      const _curGroupId = _list[i].groups?.id
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
      _list[i].mergeIndex = _curNeedMergeIndex
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

// function handleCheckAllChange(val, row, index) {
//   const _groupId = row.groups?.id
//   console.log(row, _groupId, val, listObjIdsByGroup.value[_groupId], 'handleCheckAllChange')
//   if (_groupId && listObjIdsByGroup.value[_groupId]) {
//     tableData.value.forEach((v) => {
//       if (Array.from(new Set(listObjIdsByGroup.value[_groupId])).includes(v.id)) {
//         recordTableRef?.value?.toggleRowSelection(v, val)
//       }
//     })
//   }
//   if (!val) {
//     tableData.value[index].isIndeterminateCheck = false
//   }
// }

// 合并单元格
// function spanMethod({ row, column, rowIndex, columnIndex }) {
//   if (columnIndex === 1 || columnIndex === 2) {
//     return {
//       rowspan: row.rowspan || 0,
//       colspan: 1
//     }
//   }
// }

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

// function getDisabled(row) {
//   if (selectionMode.value === selectionModeEnum.EDIT.V || listProductionLineTypeEnum.value === artifactProductLineEnum.INTELLECT.V) {
//     if (!selections.value?.length) {
//       return false
//     } else {
//       return !(row.groups?.id === selections.value[0]?.groups?.id)
//     }
//   }
//   return false
// }

// --------------------------- 操作数据 start ------------------------------

const itemInfo = ref({})
const selections = ref([])

function selectionChangeHandler(val) {
  console.log(val, 'selectionChangeHandler')
  if (val.length) {
    for (const item in listObjIdsByGroup.value) {
      const _groupsId = Number(item)
      const compareLength = Array.from(new Set(listObjIdsByGroup.value[item])).length
      const _list = val.filter((v) => v.groups.id === _groupsId)
      if (_list.length) {
        const _index = _list[0].mergeIndex
        console.log(_index, _list, listObjIdsByGroup.value[item], 'selectionChangeHandler_index')
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
  fetchLineType()
  fetchGroup()
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
  if (selectionMode.value === selectionModeEnum.EDIT.V && !selections.value?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  assembleVisible.value = true
}

// --------------------------- 部件排产 end --------------------------------

function handleTaskIssueSuccess() {
  fetchLineType()
  fetchGroup()
  emit('refresh')
}

// function handleStructureChange() {
//   fetchGroup()
//   fetch()
// }
</script>

<style lang="scss" scoped>
.hidden-table-check-all {
  ::v-deep(.el-table__header .el-table-column--selection .el-checkbox) {
    display: none !important;
  }
}
</style>
