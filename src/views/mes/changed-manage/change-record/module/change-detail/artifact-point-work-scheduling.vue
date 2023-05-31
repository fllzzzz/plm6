<template>
  <el-card>
    <el-divider><span class="title" style="padding: 5px 50px">构件点工任务列表</span></el-divider>
    <common-button type="success" size="mini" style="margin-bottom: 10px" @click="addIssuesList">加入下发列表</common-button>
    <common-table
      :header-cell-style="() => `background:#fff;font-weight: bold;color: #333333;`"
      :data="needSchedulingList"
      :cell-class-name="wrongCellMask"
      style="width: 100%; margin-bottom: 50px"
      @selection-change="handleSelectionChange"
    >
      <el-table-column type="selection" align="center" width="60" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column label="区域" prop="area.name" show-overflow-tooltip align="center" min-width="100" />
      <el-table-column label="名称" prop="name" show-overflow-tooltip align="center" min-width="100" />
      <el-table-column label="原构件信息" align="center">
        <el-table-column label="编号" prop="oldSerialNumber" show-overflow-tooltip align="center" min-width="100" />
      </el-table-column>
      <el-table-column label="变更后构件信息" align="center">
        <el-table-column label="编号" prop="serialNumber" show-overflow-tooltip align="center" min-width="100" />
        <el-table-column label="规格" prop="specification" show-overflow-tooltip align="center" min-width="120" />
        <el-table-column label="材质" prop="material" show-overflow-tooltip align="center" min-width="100" />
        <el-table-column label="长度(mm)" prop="length" show-overflow-tooltip align="center" min-width="100" />
        <el-table-column label="单净重（kg）" prop="netWeight" show-overflow-tooltip align="center" min-width="100" />
      </el-table-column>
      <el-table-column prop="needSchedulingQuantity" :show-overflow-tooltip="true" label="数量" min-width="90" align="center">
        <template #default="{ row: { sourceRow: row } }">
          <el-input-number
            v-model="row.needSchedulingQuantity"
            :step="1"
            :min="0"
            :max="row.originQuantity"
            :precision="0"
            size="mini"
            controls-position="right"
            style="width: 100%"
          />
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="groupsId" label="生产组" min-width="150px" align="center">
        <template #default="{ row: { sourceRow: row }, $index }">
          <el-cascader
            v-model="row.groupsId"
            :options="groupsObj.list"
            :props="{ value: 'id', label: 'name', children: 'children', expandTrigger: 'hover', emitPath: false }"
            filterable
            clearable
            style="width: 100%"
            :placeholder="$index === 0 ? '请选择生产组' : '同上'"
            @change="handleGroupsChange($event, row, $index)"
          />
        </template>
      </el-table-column>
      <el-table-column prop="askCompleteTime" label="要求完成日期" align="center" min-width="130px">
        <template #default="{ row: { sourceRow: row }, $index }">
          <el-date-picker
            v-model="row.askCompleteTime"
            type="date"
            size="mini"
            value-format="x"
            style="width: 100%"
            :disabledDate="(v) => moment(v).valueOf() < moment().subtract(1, 'days').valueOf()"
            :placeholder="$index === 0 ? '需求完成日期' : '同上'"
            @change="handleAskCompleteTimeChange($event, row, $index)"
          />
        </template>
      </el-table-column>
    </common-table>
    <el-divider><span class="title" style="padding: 5px 50px">构件点工下发列表</span></el-divider>
    <common-table
      :header-cell-style="() => `background:#fff;font-weight: bold;color: #333333;`"
      :data="selectedSchedulingList"
      style="width: 100%"
    >
      <el-table-column width="60">
        <template #default="{ row }">
          <common-button type="danger" size="mini" @click="handleDelRow(row)">
            <ElDelete />
          </common-button>
        </template>
      </el-table-column>
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column label="区域" prop="area.name" show-overflow-tooltip align="center" min-width="100" />
      <el-table-column label="名称" prop="name" show-overflow-tooltip align="center" min-width="100" />
      <el-table-column label="原构件信息" align="center">
        <el-table-column label="编号" prop="oldSerialNumber" show-overflow-tooltip align="center" min-width="100" />
      </el-table-column>
      <el-table-column label="变更后构件信息" align="center">
        <el-table-column label="编号" prop="serialNumber" show-overflow-tooltip align="center" min-width="100" />
        <el-table-column label="规格" prop="specification" show-overflow-tooltip align="center" min-width="120" />
        <el-table-column label="材质" prop="material" show-overflow-tooltip align="center" min-width="100" />
        <el-table-column label="长度(mm)" prop="length" show-overflow-tooltip align="center" min-width="100" />
        <el-table-column label="单净重（kg）" prop="netWeight" show-overflow-tooltip align="center" min-width="100" />
      </el-table-column>
      <el-table-column label="数量" prop="quantity" show-overflow-tooltip align="center" width="90" />
    </common-table>
  </el-card>
</template>

<script setup>
import { ref, inject, watchEffect } from 'vue'
import { manualFetchGroupsTree } from '@compos/mes/scheduling/use-scheduling-groups'
import { componentTypeEnum } from '@enum-ms/mes'
import { ElMessage } from 'element-plus'
import moment from 'moment'
import { deepClone } from '@/utils/data-type'
import useTableValidate from '@compos/form/use-table-validate'

const productType = componentTypeEnum.ARTIFACT.V
const groupsObj = ref({})
const selectedRows = ref([])
const needSchedulingList = ref([])
const selectedSchedulingList = ref([])
const schedulingInfo = inject('schedulingInfo')
const schedulingHandle = inject('schedulingHandle')

const tableRules = {
  needSchedulingQuantity: [{ required: true, message: '请填写数量', trigger: 'blur' }],
  groupsId: [{ required: true, message: '请选择生产组', trigger: 'change' }],
  askCompleteTime: [{ required: true, message: '请选择需求完成日期', trigger: 'change' }]
}
const ditto = new Map([
  ['groupsId', '同上'],
  ['askCompleteTime', '同上']
])
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

init()

watchEffect(() => {
  const _needList = []
  const _selectedList = []
  const originList = deepClone(schedulingInfo.value.pointWorkTaskArtifactList)
  for (let i = 0; i < originList.length; i++) {
    const item = originList[i]
    const _key = `${item.id}_${item.areaId}`
    if (schedulingHandle.value[item.id]) {
      _needList.push(item)
    } else {
      _selectedList.push(item)
    }
  }
  // 获取需要下发的构件点工列表
  needSchedulingList.value = _needList
  // 获取确认下发的构件点工列表
  selectedSchedulingList.value = _selectedList
})

async function init() {
  // 构件点工获取所有生产组
  groupsObj.value = await manualFetchGroupsTree(
    {
      productType
    },
    true
  )
}

function handleGroupsChange(val, row, index) {
  if (index !== 0 && !val) {
    row.groupsId = '同上'
  }
}

function handleAskCompleteTimeChange(val, row, index) {
  if (index !== 0 && !val) {
    row.askCompleteTime = '同上'
  }
}

// 选择构件点工列表
function handleSelectionChange(val) {
  selectedRows.value = val
}

// 添加下发列表
function addIssuesList() {
  if (!selectedRows.value.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  const { validResult, dealList } = tableValidate(selectedRows.value)
  if (validResult) {
    const _copyList = deepClone(dealList)
    cleanUpData(_copyList)
    _copyList.forEach((item) => {
      const _key = `${item.id}`
      const handleKey = `${item.groupsId}_${item.askCompleteTime}`
      if (!schedulingHandle.value[_key]) {
        schedulingHandle.value[_key] = {
          ...item,
          handleObj: {
            [handleKey]: {
              groupsId: item.groupsId,
              askCompleteTime: item.askCompleteTime,
              needSchedulingQuantity: item.needSchedulingQuantity
            }
          }
        }
      } else {
        if (!schedulingHandle.value[_key].handleObj[handleKey]) {
          schedulingHandle.value[_key].handleObj[handleKey] = {
            groupsId: item.groupsId,
            askCompleteTime: item.askCompleteTime,
            needSchedulingQuantity: item.needSchedulingQuantity
          }
        } else {
          schedulingHandle.value[_key].handleObj[handleKey].needSchedulingQuantity = item.needSchedulingQuantity
        }
      }
    })
  }
}

// 删除下发列表
function handleDelRow(row) {
  if (schedulingHandle.value[row.id]) {
    delete schedulingHandle.value[row.id]
  }
}
</script>

<style lang="scss" scoped></style>
