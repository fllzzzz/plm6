<template>
  <el-card>
    <el-divider><span class="title" style="padding: 5px 50px">部件排产列表</span></el-divider>
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
      <el-table-column label="变更构件编号" prop="artifactSerialNumber" show-overflow-tooltip align="center" min-width="100" />
      <el-table-column label="变更部件编号" prop="serialNumber" show-overflow-tooltip align="center" min-width="100" />
      <el-table-column label="规格" prop="specification" show-overflow-tooltip align="left" min-width="120" />
      <el-table-column label="长度(mm)" prop="length" show-overflow-tooltip align="center" min-width="110" />
      <el-table-column label="单重(kg)" prop="netWeight" show-overflow-tooltip align="center" min-width="110" />
      <el-table-column prop="needSchedulingQuantity" :show-overflow-tooltip="true" label="数量" min-width="90" align="center">
        <template #default="{ row: { sourceRow: row } }">
          <el-tooltip
            class="item"
            effect="dark"
            :content="`变更清单数量：${row.originQuantity} ${
              row.maxQuantity < row.originQuantity ? '，已加入下发数量：' + (row.originQuantity - row.maxQuantity) : ''
            }`"
            placement="top"
          >
            <el-input-number
              v-model="row.needSchedulingQuantity"
              :step="1"
              :min="0"
              :max="row.maxQuantity"
              :precision="0"
              size="mini"
              controls-position="right"
              style="width: 100%"
            />
          </el-tooltip>
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
    <el-divider><span class="title" style="padding: 5px 50px">部件下发列表</span></el-divider>
    <common-table
      :header-cell-style="() => `background:#fff;font-weight: bold;color: #333333;`"
      :data="selectedSchedulingList"
      style="width: 100%"
    >
      <el-table-column width="60" align="center">
        <template #default="{ row }">
          <common-button type="danger" icon="el-icon-delete" size="mini" style="padding: 6px" @click="handleDelRow(row)" />
        </template>
      </el-table-column>
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column label="区域" prop="area.name" show-overflow-tooltip align="center" min-width="100" />
      <el-table-column label="变更构件编号" prop="artifactSerialNumber" show-overflow-tooltip align="center" min-width="100" />
      <el-table-column label="变更部件编号" prop="serialNumber" show-overflow-tooltip align="center" min-width="100" />
      <el-table-column label="规格" prop="specification" show-overflow-tooltip align="left" min-width="120" />
      <el-table-column label="长度(mm)" prop="length" show-overflow-tooltip align="center" min-width="110" />
      <el-table-column label="单重(kg)" prop="netWeight" show-overflow-tooltip align="center" min-width="110" />
      <el-table-column label="数量" prop="needSchedulingQuantity" show-overflow-tooltip align="center" min-width="90" />
      <el-table-column label="生产组" prop="groupsId" show-overflow-tooltip align="center" min-width="150">
        <template #default="{ row }">
          <span v-if="groupsObj?.obj?.[row.groupsId]">
            {{ groupsObj?.obj?.[row.groupsId]?.workshop?.name }}/{{ groupsObj?.obj?.[row.groupsId]?.productionLine?.name }}/{{
              groupsObj?.obj?.[row.groupsId]?.groupsName
            }}
          </span>
        </template>
      </el-table-column>
      <el-table-column label="要求完成日期" prop="askCompleteTime" show-overflow-tooltip align="center" min-width="130">
        <template #default="{ row }">
          <span>{{ row.askCompleteTime ? parseTime(row.askCompleteTime, '{y}-{m}-{d}') : '-' }}</span>
        </template>
      </el-table-column>
    </common-table>
  </el-card>
</template>

<script setup>
import { ref, inject, watchEffect } from 'vue'
import { manualFetchGroupsTree } from '@compos/mes/scheduling/use-scheduling-groups'
import { componentTypeEnum } from '@enum-ms/mes'
import { ElMessage } from 'element-plus'
import moment from 'moment'
import { deepClone, isBlank } from '@/utils/data-type'
import { parseTime } from '@/utils/date'
import useTableValidate from '@compos/form/use-table-validate'

const productType = componentTypeEnum.ASSEMBLE.V
const groupsObj = ref({})
const selectedRows = ref([])
const needSchedulingList = ref([])
const selectedSchedulingList = ref([])
const schedulingInfo = inject('schedulingInfo')
const schedulingHandle = inject('schedulingHandle')
const keyPath = 'assemble'

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
  const originList = deepClone(schedulingInfo.value.assembleList)

  for (let i = 0; i < originList.length; i++) {
    const item = originList[i]
    let surplusQuantity = item.originQuantity
    // const _key = `${item.id}_${item.areaId}`
    if (!schedulingHandle.value?.[keyPath]?.[item.rowKey]) {
      if (_needList.length) {
        item.groupsId = item.groupsId ? item.groupsId : '同上'
        item.askCompleteTime = item.askCompleteTime ? item.askCompleteTime : '同上'
      }
      _needList.push({
        ...item,
        needSchedulingQuantity: surplusQuantity,
        maxQuantity: surplusQuantity
      })
    } else {
      for (const key in schedulingHandle.value[keyPath][item.rowKey]?.handleObj) {
        if (Object.hasOwnProperty.call(schedulingHandle.value[keyPath][item.rowKey].handleObj, key)) {
          const element = schedulingHandle.value[keyPath][item.rowKey].handleObj[key]
          _selectedList.push({
            ...item,
            ...element
          })
          surplusQuantity -= element.needSchedulingQuantity
        }
      }
      if (surplusQuantity > 0) {
        if (_needList.length) {
          item.groupsId = item.groupsId ? item.groupsId : '同上'
          item.askCompleteTime = item.askCompleteTime ? item.askCompleteTime : '同上'
        }
        _needList.push({
          ...item,
          needSchedulingQuantity: surplusQuantity,
          maxQuantity: surplusQuantity
        })
      }
    }
  }
  // 获取需要下发的部件排产列表
  needSchedulingList.value = _needList
  // 获取确认下发的部件排产列表
  selectedSchedulingList.value = _selectedList
  schedulingHandle.value[keyPath + 'NeedSchedulingList'] = _needList
})

async function init() {
  // 部件排产获取所有生产组
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

// 选择部件排产列表
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
      const _key = item.rowKey
      // const handleKey = `${item.groupsId}_${item.askCompleteTime}`
      const handleKey = `${item.groupsId}`
      if (!schedulingHandle.value?.[keyPath]?.[_key]) {
        schedulingHandle.value[keyPath][_key] = {
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
        schedulingHandle.value[keyPath][_key].needSchedulingQuantity += item.needSchedulingQuantity
        if (!schedulingHandle.value?.[keyPath]?.[_key].handleObj[handleKey]) {
          schedulingHandle.value[keyPath][_key].handleObj[handleKey] = {
            groupsId: item.groupsId,
            askCompleteTime: item.askCompleteTime,
            needSchedulingQuantity: item.needSchedulingQuantity
          }
        } else {
          schedulingHandle.value[keyPath][_key].handleObj[handleKey].needSchedulingQuantity += item.needSchedulingQuantity
          schedulingHandle.value[keyPath][_key].handleObj[handleKey].askCompleteTime = Math.max(
            item.askCompleteTime,
            schedulingHandle.value[keyPath][_key].handleObj[handleKey].askCompleteTime
          )
        }
      }
    })
  }
}

// 删除下发列表
function handleDelRow(row) {
  // const handleKey = `${row.groupsId}_${row.askCompleteTime}`
  const handleKey = `${row.groupsId}`
  if (schedulingHandle.value[keyPath][row.rowKey]) {
    // 删除handleObj中的数据
    if (schedulingHandle.value[keyPath][row.rowKey].handleObj[handleKey]) {
      schedulingHandle.value[keyPath][row.rowKey].needSchedulingQuantity -=
        schedulingHandle.value[keyPath][row.rowKey].handleObj[handleKey].needSchedulingQuantity
      delete schedulingHandle.value[keyPath][row.rowKey].handleObj[handleKey]
    }
    // 如果handleObj为空，删除整个对象
    if (isBlank(schedulingHandle.value[keyPath][row.rowKey].handleObj)) {
      delete schedulingHandle.value[keyPath][row.rowKey]
    }
  }
}
</script>

<style lang="scss" scoped></style>
