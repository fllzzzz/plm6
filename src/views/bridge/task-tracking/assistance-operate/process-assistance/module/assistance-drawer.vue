<template>
  <common-drawer
    ref="drawerRef"
    :title="`工序协同【${props.info?.process?.name}】`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="80%"
  >
    <template #titleAfter>
      <el-tag effect="plain" v-if="crud.query.taskTypeEnum !== bridgeTaskTypeEnum.MACHINE_PART.V">
        原生产组：{{ info.workshop?.name }}>{{ info.productionLine?.name }}>{{ info.groups?.name }}
      </el-tag>
    </template>
    <template #titleRight>
      <common-button v-permission="permission.del" size="mini" type="danger" @click="toBatchDelete">批量删除【协同班组】</common-button>
      <common-button v-permission="permission.save" size="mini" type="primary" @click="previewIt">预览并保存</common-button>
    </template>
    <template #content>
      <div class="head-container">
        <el-input
          v-model="query.serialNumber"
          size="small"
          placeholder="输入编号搜索"
          style="width: 170px"
          class="filter-item"
          clearable
          @keyup.enter="fetch"
        />
        <el-input
          v-model="query.specification"
          size="small"
          placeholder="输入规格搜索"
          style="width: 170px"
          class="filter-item"
          clearable
          @keyup.enter="fetch"
        />
        <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="fetch">搜索</common-button>
        <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
          重置
        </common-button>
      </div>
      <common-table
        :data="tableData"
        v-loading="tableLoading"
        :max-height="maxHeight"
        :cell-class-name="wrongCellMask"
        :data-format="dataFormat"
        @selection-change="handleSelectChange"
        style="width: 100%"
      >
        <el-table-column type="selection" width="55" align="center" :selectable="selectable" />
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column
          v-if="crud.query.taskTypeEnum !== bridgeTaskTypeEnum.MACHINE_PART.V"
          prop="areaName"
          :show-overflow-tooltip="true"
          label="区域"
          min-width="100px"
          align="center"
        />
        <el-table-column
          v-if="crud.query.taskTypeEnum === bridgeTaskTypeEnum.MACHINE_PART.V"
          prop="cutNumber"
          :show-overflow-tooltip="true"
          label="切割指令号"
          min-width="100px"
          align="center"
        />
        <el-table-column
          v-if="crud.query.taskTypeEnum === bridgeTaskTypeEnum.CELL.V"
          prop="attributeType"
          :show-overflow-tooltip="true"
          label="属性"
          width="90"
          align="center"
        >
          <template #default="{ row }">
            <el-tag :type="row.attributeType === '单元件' ? 'warning' : 'success'">{{ row.attributeType }}</el-tag>
          </template>
        </el-table-column>
        <el-table-column
          v-if="crud.query.taskTypeEnum !== bridgeTaskTypeEnum.MACHINE_PART.V"
          prop="serialNumber"
          :show-overflow-tooltip="true"
          label="编号"
          min-width="100px"
          align="center"
        />
        <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="100px" align="center" />
        <el-table-column
          v-if="crud.query.taskTypeEnum !== bridgeTaskTypeEnum.MACHINE_PART.V"
          prop="length"
          :show-overflow-tooltip="true"
          label="长度(mm)"
          width="100px"
          align="center"
        />
        <el-table-column align="center" prop="totalTaskMete.quantity" :show-overflow-tooltip="true" label="任务（件/kg）">
          <template #default="{ row }">
            <span>{{ row.totalTaskMete?.quantity || 0 }}/{{ row.totalTaskMete?.netWeight || 0 }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" prop="totalCompleteMete.quantity" :show-overflow-tooltip="true" label="完成（件/kg）">
          <template #default="{ row }">
            <span>{{ row.totalCompleteMete?.quantity || 0 }}/{{ row.totalCompleteMete?.netWeight || 0 }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="crud.query.taskTypeEnum === bridgeTaskTypeEnum.MACHINE_PART.V"
          align="center"
          prop="askCompleteTime"
          :show-overflow-tooltip="true"
          label="计划完成日期"
          width="120px"
        />
        <el-table-column
          v-if="crud.query.taskTypeEnum === bridgeTaskTypeEnum.MACHINE_PART.V"
          prop="group.name"
          :show-overflow-tooltip="true"
          label="原生产组"
          min-width="180px"
        >
          <template #default="{ row }">
            <span>{{ row.workshop?.name }}>{{ row.productionLine?.name }}>{{ row.groups?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="groupsId" label="协同生产组" min-width="150px" align="center">
          <template #default="{ row: { sourceRow: row }, $index }">
            <el-cascader
              v-model="row.groupsId"
              :options="classIdGroupsObj[row.configId].list"
              :props="{ value: 'id', label: 'name', children: 'children', expandTrigger: 'hover', emitPath: false }"
              show-all-levels
              :disabled="row.isComplete"
              filterable
              clearable
              :placeholder="$index === 0 ? '请选择生产组' : '同上'"
              style="width: 100%"
              @change="handleGroupsChange($event, row, $index)"
            />
          </template>
        </el-table-column>
      </common-table>
      <assistance-preview v-model:visible="previewVisible" :info="info" :list="submitList" @success="handleSuccess" />
    </template>
  </common-drawer>
</template>

<script setup>
import { detail, del } from '@/api/bridge/bridge-task-tracking/assistance-operate/process-assistance'
import { defineProps, defineEmits, ref, inject } from 'vue'
import { ElMessage, ElNotification, ElMessageBox } from 'element-plus'

import { bridgeTaskTypeEnum } from '@enum-ms/bridge'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useTableValidate from '@compos/form/use-table-validate'
import { manualFetchGroupsTree } from '@compos/mes/scheduling/use-scheduling-groups'

import assistancePreview from './assistance-preview'

const drawerRef = ref()
const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: resetQuery })

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

const dataFormat = ref([['askCompleteTime', ['parse-time', '{y}-{m}-{d}']]])

const crud = inject('crud')
const permission = inject('permission')
const query = ref({})
const tableData = ref([])
const tableLoading = ref(false)
const classIdGroupsObj = ref({})
const selections = ref([])
const submitList = ref([])
const previewVisible = ref(false)

const tableRules = {
  groupsId: [{ required: true, message: '请选择生产组', trigger: 'change' }]
}
const ditto = new Map([['groupsId', '同上']])
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

function selectable(row, rowIndex) {
  return !row.isComplete
}

function resetQuery() {
  query.value = {}
  fetch()
}

function handleSelectChange(val) {
  selections.value = val
}

function handleGroupsChange(val, row, index) {
  if (index !== 0 && !val) {
    row.groupsId = '同上'
  }
}

async function fetch() {
  try {
    tableLoading.value = true
    tableData.value = []
    classIdGroupsObj.value = {}
    const { content } = await detail({
      taskOrderId: props.info?.taskOrderId,
      taskTypeEnum:
        props.info?.taskTypeEnum & bridgeTaskTypeEnum.CELL.V
          ? props.info?.taskTypeEnum | bridgeTaskTypeEnum.PARENT_PART.V
          : props.info?.taskTypeEnum,
      processId: props.info?.process.id,
      ...query.value
    })
    for (let i = 0; i < content.length; i++) {
      const v = content[i]
      v.attributeType = v.taskTypeEnum === bridgeTaskTypeEnum.CELL.V ? '单元件' : '套料'
      v.areaName = v.area?.name
      v.configId = v.config?.id
      v.groupsId = v.groups?.id
      v.isComplete = Boolean(
        v.totalCompleteMete?.quantity && v.totalTaskMete?.quantity && v.totalCompleteMete?.quantity === v.totalTaskMete?.quantity
      )
      if (!classIdGroupsObj.value[v.configId]) {
        let res = {}
        if (crud.query.taskTypeEnum === bridgeTaskTypeEnum.MACHINE_PART.V) {
          res = await manualFetchGroupsTree({
            productType: props.info?.taskTypeEnum,
            disabledIds: (v?.groups?.id && [v?.groups?.id]) || []
          })
        } else {
          res = await manualFetchGroupsTree({
            productType: props.info?.taskTypeEnum,
            boxClassId: v.configId,
            disabledIds: (props.info?.groups?.id && [props.info?.groups?.id]) || [],
            _factoryIds: (v.factoryId && [v.factoryId]) || []
          })
        }
        classIdGroupsObj.value[v.configId] = res
      }
      if (i > 0 && !v.groupsId) {
        v.groupsId = '同上'
      }
      tableData.value.push(v)
    }
  } catch (error) {
    console.log('可变更的任务工单详情列表获取失败', error)
  } finally {
    tableLoading.value = false
  }
}

function previewIt() {
  if (!selections.value?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  const _list = selections.value.map((v) => v)
  const { validResult, dealList } = tableValidate(_list)
  if (validResult) {
    cleanUpData(dealList) // 同上赋值
    submitList.value = dealList.map((v, i) => {
      return {
        ...v,
        assistance: {
          ...classIdGroupsObj.value[v.configId].obj[v.groupsId]
        }
      }
    })
  } else {
    return validResult
  }

  previewVisible.value = true
}

function toBatchDelete() {
  if (!selections.value?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  ElMessageBox.confirm(`是否确认删除所选择的班组协同？`, '提示', {
    confirmButtonText: '确认',
    cancelButtonText: '取消',
    type: 'warning'
  }).then(async () => {
    try {
      const ids = selections.value.map((v) => v.id)
      await del({ ids })
      ElNotification({ title: '班组删除成功', type: 'success' })
      fetch()
    } catch (error) {
      console.log('班组删除失败', error)
    }
  })
}

function handleSuccess() {
  fetch()
}
</script>

<style scoped>
.tip {
  display: inline-block;
  color: red;
  text-decoration: underline;
  margin-bottom: 10px;
}
</style>
