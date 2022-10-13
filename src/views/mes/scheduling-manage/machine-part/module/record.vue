<template>
  <common-drawer ref="drawerRef" title="零件排产记录" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="100%">
    <template #titleRight> </template>
    <template #content>
      <div class="head-container">
        <common-radio-button
          v-model="queryVO.issueStatusEnum"
          :options="nestingStatusEnum.ENUM"
          show-option-all
          type="enum"
          size="small"
          class="filter-item"
          @change="fetch"
        />
        <common-button class="filter-item" style="float: right" size="mini" type="success" @click="toBatchIssue">套料下发</common-button>
        <common-button class="filter-item" style="float: right" size="mini" type="danger" @click="toBatchDel">批量删除</common-button>
      </div>
      <common-table
        v-loading="tableLoading"
        :data="tableData"
        :cell-class-name="wrongCellMask"
        :max-height="maxHeight"
        :stripe="false"
        :data-format="dataFormat"
        style="width: 100%"
        @selection-change="selectionChangeHandler"
      >
        <el-table-column type="selection" width="55" align="center" class="selection" :selectable="selectable" />
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="date" :show-overflow-tooltip="true" label="排产日期" width="110" align="center" />
        <el-table-column prop="userName" :show-overflow-tooltip="true" label="操作人" min-width="100" align="center" />
        <el-table-column prop="orderNumber" :show-overflow-tooltip="true" label="套料单号" min-width="120" align="center" />
        <!-- <el-table-column prop="layWayConfigName" :show-overflow-tooltip="true" label="下料方式" min-width="100" align="center">
          <template #default="{ row }">
            <el-tag :type="row.materialTypeEnum === materialTypeEnum.MANMADE_BLANKING.V ? '' : 'warning'" effect="plain">{{
              row.layWayConfigName
            }}</el-tag>
          </template>
        </el-table-column> -->
        <el-table-column prop="thick" :show-overflow-tooltip="true" label="板厚（mm）" min-width="100" align="center">
          <!-- <template #header>
            <el-tooltip class="item" effect="light" :content="`双击板厚可查看清单详情`" placement="top">
              <div style="display: inline-block">
                <span>板厚（mm）</span>
                <i class="el-icon-info" />
              </div>
            </el-tooltip>
          </template> -->
          <template #default="{ row }">
            <!-- <span class="tc-primary" style="cursor: pointer" @dblclick="toDetail(row)">{{ row.thick }}</span> -->
            <span>{{ row.thick }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="material" :show-overflow-tooltip="true" label="材质" min-width="100" align="center">
          <template #default="{ row }">
            <span>{{ row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="quantity" :show-overflow-tooltip="true" label="数量（件）" min-width="90" align="center">
          <template #default="{ row }">
            <span>{{ row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="totalNetWeight" :show-overflow-tooltip="true" label="重量（kg）" min-width="90" align="center">
          <template #default="{ row }">
            <span>{{ row.totalNetWeight }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="cutConfigId" :show-overflow-tooltip="true" label="切割方式" min-width="90" align="center">
          <template #default="{ row: { sourceRow: row } }">
            <cut-config-select v-if="row.issueStatusEnum === issueStatusEnum.NOT_NESTING.V" v-model="row.cutConfigId" clearable />
            <span v-else>{{ row.cutConfigName }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="issueStatusEnum" :show-overflow-tooltip="true" label="下发状态" width="90" align="center">
          <template #default="{ row: { sourceRow: row } }">
            <el-tag v-if="row.issueStatusEnum && issueStatusEnum.V[row.issueStatusEnum]" :type="issueStatusEnum.V[row.issueStatusEnum].T">
              {{ issueStatusEnum.VL[row.issueStatusEnum] }}
            </el-tag>
          </template>
        </el-table-column>
        <!-- <el-table-column :show-overflow-tooltip="true" prop="groupsId" label="生产组" min-width="100px" align="center">
          <template #default="{ row: { sourceRow: row }, $index }">
            <el-cascader
              v-model="row.groupsId"
              :options="schedulingGroups.list"
              :props="{ value: 'id', label: 'name', children: 'children', expandTrigger: 'hover', emitPath: false }"
              filterable
              clearable
              style="width: 100%"
              :placeholder="$index === 0 ? '请选择生产组' : '同上'"
            />
          </template>
        </el-table-column>
        <el-table-column prop="askCompleteTime" :show-overflow-tooltip="true" label="完成日期" min-width="90" align="center">
          <template #default="{ row: { sourceRow: row } }">
            <el-date-picker
              v-model="row.askCompleteTime"
              type="date"
              size="mini"
              value-format="x"
              style="width: 100%"
              :disabledDate="(v) => moment(v).valueOf() < moment().subtract(1, 'days').valueOf()"
              placeholder="完成日期"
            />
          </template>
        </el-table-column> -->
        <el-table-column label="操作" width="150" align="center">
          <template #default="{ row: { sourceRow: row } }">
            <common-button type="primary" @click="toDetail(row)">查看</common-button>
            <el-popconfirm
              v-if="row.issueStatusEnum === issueStatusEnum.NOT_NESTING.V"
              confirm-button-text="确定"
              cancel-button-text="取消"
              title="确定删除吗?"
              @confirm="rowDelete(row)"
            >
              <template #reference>
                <common-button size="mini" type="danger">删除</common-button>
              </template>
            </el-popconfirm>
            <!-- <el-popconfirm confirm-button-text="确定" cancel-button-text="取消" title="确定删除吗?" @confirm="rowTask(row)">
              <template #reference>
                <common-button size="mini" type="primary">套料下发</common-button>
              </template>
            </el-popconfirm> -->
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
      <record-detail v-model:visible="detailVisible" :recordId="currentRow?.id" @del-success="handleDelSuccess"></record-detail>
    </template>
  </common-drawer>
</template>

<script setup>
import { record, del, saveNesting } from '@/api/mes/scheduling-manage/machine-part'
import { defineProps, defineEmits, ref } from 'vue'
import { ElNotification, ElMessage, ElMessageBox } from 'element-plus'

import {
  componentTypeEnum,
  machinePartSchedulingIssueStatusEnum as issueStatusEnum,
  machinePartNestingStatusEnum as nestingStatusEnum
} from '@enum-ms/mes'

import useTableValidate from '@compos/form/use-table-validate'
import { manualFetchGroupsTree } from '@compos/mes/scheduling/use-scheduling-groups'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
import cutConfigSelect from '@/components-system/base/cut-config-select.vue'
import recordDetail from './record-detail.vue'

const drawerRef = ref()
const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: resetQuery, closeHook })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetch })

const dataFormat = ref([['askCompleteTime', ['parse-time', '{y}-{m}-{d}']]])

const detailVisible = ref(false)
const currentRow = ref()
const tableData = ref([])
const tableLoading = ref(false)
const queryVO = ref({})
const closeRefreshOut = ref(false)

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.head-container'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const tableRules = {
  cutConfigId: [{ required: true, message: '请选择切割方式', trigger: 'change' }]
}
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })

function resetQuery() {
  fetch()
}

function closeHook() {
  // 有删除操作需刷新外层数据
  if (closeRefreshOut.value) {
    emit('refresh')
  }
}

async function fetch() {
  try {
    tableLoading.value = true
    await fetchGroups()
    const { content, totalElements } = await record({ ...queryVO.value, ...queryPage })
    setTotalPage(totalElements)
    const _list = content
    // let _curNeedMergeIndex = 0 // 首行为初始需要的合并行
    // let _mergeRowspan = 0
    // let _mergeQuantity = 0
    // let _mergeWeight = 0
    // for (let i = 0; i < _list.length; i++) {
    //   // 处理首行
    //   if (i === 0) {
    //     _mergeRowspan++
    //     _mergeQuantity += _list[i].schedulingQuantity
    //     _mergeWeight += _list[i].schedulingTotalNetWeight
    //   }
    //   if (i > 0) {
    //     // 与上行合并
    //     if (_list[i].groups?.id === _list[i - 1].groups?.id) {
    //       _mergeRowspan++
    //       _mergeQuantity += _list[i].schedulingQuantity
    //       _mergeWeight += _list[i].schedulingTotalNetWeight
    //       _list[_curNeedMergeIndex].rowspan = 0
    //     } else {
    //       // 不合并
    //       // 赋值已合计的信息给上一个合并行
    //       _list[_curNeedMergeIndex].rowspan = _mergeRowspan
    //       _list[_curNeedMergeIndex].mergeQuantity = _mergeQuantity
    //       _list[_curNeedMergeIndex].mergeWeight = _mergeWeight.toFixed(2)
    //       // 重置合并信息
    //       _mergeRowspan = 1
    //       _mergeQuantity = _list[i].schedulingQuantity
    //       _mergeWeight = _list[i].schedulingTotalNetWeight
    //       // 赋值当前的index
    //       _curNeedMergeIndex = i
    //     }
    //   }
    //   // 最后一行时 处理最后的_curNeedMergeIndex
    //   if (i === _list.length - 1) {
    //     _list[_curNeedMergeIndex].rowspan = _mergeRowspan
    //     _list[_curNeedMergeIndex].mergeQuantity = _mergeQuantity
    //     _list[_curNeedMergeIndex].mergeWeight = _mergeWeight.toFixed(2)
    //   }
    //   // 其他处理数据
    //   _list[i].needSchedulingQuantity = _list[i].schedulingQuantity
    // }
    tableData.value = _list
  } catch (error) {
    console.log('获取构件排产预览记录失败', error)
  } finally {
    tableLoading.value = false
  }
}

const selections = ref([])

function selectable(row, rowIndex) {
  return row.issueStatusEnum === issueStatusEnum.NOT_NESTING.V
}

function selectionChangeHandler(val) {
  selections.value = val
}

// --------------------------- 获取生产班组 start ------------------------------
const groupLoad = ref(false)
const schedulingGroups = ref({ list: [], obj: {}})

async function fetchGroups() {
  if (groupLoad.value) return
  try {
    schedulingGroups.value = await manualFetchGroupsTree({ productType: componentTypeEnum.MACHINE_PART.V })
    groupLoad.value = true
  } catch (e) {
    console.log('获取生产组的信息失败', e)
  }
}
// --------------------------- 获取生产班组 end --------------------------------

function toBatchDel() {
  if (!selections.value?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  ElMessageBox.confirm(`是否确认删除所选零件排产`, '提示', {
    confirmButtonText: '确认',
    cancelButtonText: '取消',
    type: 'warning'
  }).then(async () => {
    try {
      const ids = selections.value.map((v) => v.id)
      await del(ids)
      ElNotification({ title: '删除成功', type: 'success', duration: 3000 })
      handleDelSuccess()
    } catch (error) {
      console.log('删除失败', error)
    }
  })
}

async function rowDelete(row) {
  try {
    await del([row.id])
    ElNotification({ title: '删除成功', type: 'success', duration: 3000 })
    handleDelSuccess()
  } catch (e) {
    console.log(`删除失败`, e)
  }
}

async function toBatchIssue() {
  if (!selections.value?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  try {
    const { validResult, dealList } = tableValidate(selections.value)
    if (validResult) {
      const _resList = dealList.map((v) => {
        return {
          cutConfigId: v.cutConfigId,
          schedulingId: v.id
        }
      })
      await saveNesting(_resList)
      ElNotification({ title: '套料下发成功', type: 'success', duration: 3000 })
      fetch()
    }
  } catch (e) {
    console.log(`套料下发失败`, e)
  }
}

function handleDelSuccess() {
  closeRefreshOut.value = true
  fetch()
}

function toDetail(row) {
  currentRow.value = row
  detailVisible.value = true
}
</script>
