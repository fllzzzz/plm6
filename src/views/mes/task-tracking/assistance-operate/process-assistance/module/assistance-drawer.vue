<template>
  <common-drawer
    ref="drawerRef"
    :title="`工序协同【${props.info?.process?.name}】`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="100%"
  >
    <template #titleAfter>
      <el-tag effect="plain" v-if="props.info?.taskTypeEnum !== taskTypeENUM.MACHINE_PART.V">
        原生产组：{{ info.workshop?.name }}>{{ info.productionLine?.name }}>{{ info.groups?.name }}
      </el-tag>
    </template>
    <!-- <template #titleRight>
      <common-button size="mini" type="danger" @click="toBatchDelete">批量删除【协同班组】</common-button>
      <common-button size="mini" type="primary" @click="previewIt">预览并保存</common-button>
    </template> -->
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
        :data-format="dataFormat"
        :show-empty-symbol="false"
        @selection-change="handleSelectChange"
        style="width: 100%"
      >
        <!-- <el-table-column type="selection" width="55" align="center" :selectable="selectable" /> -->
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column
          v-if="props.info?.taskTypeEnum !== taskTypeENUM.MACHINE_PART.V"
          prop="areaName"
          :show-overflow-tooltip="true"
          label="区域"
          min-width="100px"
          align="center"
        />
        <el-table-column
          v-if="props.info?.taskTypeEnum === taskTypeENUM.MACHINE_PART.V"
          prop="cutNumber"
          :show-overflow-tooltip="true"
          label="切割指令号"
          min-width="100px"
          align="center"
        />
        <el-table-column
          v-if="props.info?.taskTypeEnum === taskTypeENUM.MACHINE_PART.V"
          prop="attributeType"
          :show-overflow-tooltip="true"
          label="属性"
          width="90"
          align="center"
        >
          <template #default="{ row }">
            <el-tag :type="row.attributeType === '部件' ? 'warning' : 'success'">{{ row.attributeType }}</el-tag>
          </template>
        </el-table-column>
        <el-table-column
          v-if="props.info?.taskTypeEnum !== taskTypeENUM.MACHINE_PART.V"
          prop="serialNumber"
          :show-overflow-tooltip="true"
          label="编号"
          min-width="100px"
          align="center"
        />
        <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="100px" align="center" />
        <el-table-column
          v-if="props.info?.taskTypeEnum !== taskTypeENUM.MACHINE_PART.V"
          prop="length"
          :show-overflow-tooltip="true"
          label="长度(mm)"
          width="100px"
          align="center"
        />
        <el-table-column align="center" prop="totalTaskMete.quantity" :show-overflow-tooltip="true" label="任务数（件）">
          <template #default="{ row }">
            <span>{{ row.totalTaskMete?.quantity || 0 }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" prop="totalTaskMete.netWeight" :show-overflow-tooltip="true" label="任务总净重（kg）">
          <template #default="{ row }">
            <span>{{ row.totalTaskMete?.netWeight || 0 }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" prop="totalTaskMete.grossWeight" :show-overflow-tooltip="true" label="任务总毛重（kg）">
          <template #default="{ row }">
            <span>{{ row.totalTaskMete?.grossWeight || 0 }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" prop="totalCompleteMete.quantity" :show-overflow-tooltip="true" label="完成数（件）">
          <template #default="{ row }">
            <span>{{ row.totalCompleteMete?.quantity || 0 }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" prop="totalCompleteMete.netWeight" :show-overflow-tooltip="true" label="完成总净重（kg）">
          <template #default="{ row }">
            <span>{{ row.totalCompleteMete?.netWeight || 0 }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" prop="totalCompleteMete.grossWeight" :show-overflow-tooltip="true" label="完成总毛重（kg）">
          <template #default="{ row }">
            <span>{{ row.totalCompleteMete?.grossWeight || 0 }}</span>
          </template>
        </el-table-column>
        <!-- <el-table-column
          align="center"
          prop="askCompleteTime"
          :show-overflow-tooltip="true"
          label="计划完成日期"
          width="120px"
        /> -->
        <!-- <el-table-column
          prop="group.name"
          :show-overflow-tooltip="true"
          label="原生产组"
          min-width="180px"
        >
          <template #default="{ row }">
            <span>{{ row.workshop?.name }}>{{ row.productionLine?.name }}>{{ row.groups?.name }}</span>
          </template>
        </el-table-column> -->
        <el-table-column align="center" prop="totalAssistQuantity" :show-overflow-tooltip="true" label="已协同数量">
          <template #default="{ row }">
            <span>{{ row.totalAssistQuantity || 0 }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" prop="totalCanAssistMete.quantity" :show-overflow-tooltip="true" label="可协同数量">
          <template #default="{ row }">
            <span>{{ row.totalCanAssistMete?.quantity }}</span>
          </template>
        </el-table-column>
        <!-- <el-table-column :show-overflow-tooltip="true" prop="groupsId" label="协同生产组" min-width="150px" align="center">
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
        </el-table-column> -->
        <el-table-column align="center" :show-overflow-tooltip="true" label="操作">
          <template #default="{ row }">
            <common-button size="mini" type="success" @click.stop="deal(row)">办理</common-button>
          </template>
        </el-table-column>
      </common-table>
      <assistance-preview v-model:visible="previewVisible" :info="info" :list="submitList" @success="handleSuccess" />
      <assistance-detail-form
        v-model:visible="detailVisible"
        :detail-data="detailData"
        :detail-list="detailList"
        :classIdGroupsObj="classIdGroupsObj"
        @success="detailSuccess"
      />
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/mes/task-tracking/assistance-operate/process-assistance'
import { defineProps, defineEmits, ref } from 'vue'
// import { ElMessage, ElNotification, ElMessageBox } from 'element-plus'

import { taskTypeENUM } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
// import useTableValidate from '@compos/form/use-table-validate'
import { manualFetchGroupsTree } from '@compos/mes/scheduling/use-scheduling-groups'

import assistancePreview from './assistance-preview'
import assistanceDetailForm from './assistance-detail-form'

const drawerRef = ref()
const detailVisible = ref(false)
const detailData = ref({})
const detailList = ref([])
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

// const permission = inject('permission')
const query = ref({})
const tableData = ref([])
const tableLoading = ref(false)
const classIdGroupsObj = ref({})
const selections = ref([])
const submitList = ref([])
const previewVisible = ref(false)
const pIds = ref([])

function resetQuery() {
  query.value = {}
  fetch()
}

function handleSelectChange(val) {
  selections.value = val
}

async function fetch() {
  pIds.value = [props.info?.groups?.id]
  try {
    tableLoading.value = true
    tableData.value = []
    classIdGroupsObj.value = {}
    const { content } = await detail({
      taskOrderId: props.info?.taskOrderId,
      taskTypeEnum:
        props.info?.taskTypeEnum & taskTypeENUM.ASSEMBLE.V
          ? props.info?.taskTypeEnum | taskTypeENUM.PARENT_PART.V
          : props.info?.taskTypeEnum,
      processId: props.info?.process.id,
      ...query.value
    })
    for (let i = 0; i < content.length; i++) {
      const v = content[i]
      v.attributeType = v.taskTypeEnum === taskTypeENUM.ASSEMBLE.V ? '部件' : '套料'
      v.areaName = v.area?.name
      v.configId = v.config?.id
      v.isComplete = Boolean(
        v.totalCompleteMete?.quantity && v.totalTaskMete?.quantity && v.totalCompleteMete?.quantity === v.totalTaskMete?.quantity
      )
      if (v?.mesBuildingAssistDetailDTOS?.length > 0) {
        v?.mesBuildingAssistDetailDTOS.forEach(async (p) => {
          p.groupsId = p?.groups?.id
          pIds.value.push(p.groupsId)
          if (!classIdGroupsObj.value[v?.configId]) {
            let res = {}
            if (props.info?.taskTypeEnum === taskTypeENUM.MACHINE_PART.V) {
              res = await manualFetchGroupsTree({
                productType: props.info?.taskTypeEnum,
                disabledIds: pIds.value || []
              })
            } else {
              res = await manualFetchGroupsTree({
                productType: props.info?.taskTypeEnum,
                structureClassId: v.configId,
                disabledIds: pIds.value || [],
                _factoryIds: (v.factoryId && [v.factoryId]) || []
              })
            }
            classIdGroupsObj.value[v.configId] = res
          }
        })
      } else {
        if (!classIdGroupsObj.value[v?.configId]) {
          let res = {}
          if (props.info?.taskTypeEnum === taskTypeENUM.MACHINE_PART.V) {
            res = await manualFetchGroupsTree({
              productType: props.info?.taskTypeEnum,
              disabledIds: pIds.value || []
            })
          } else {
            res = await manualFetchGroupsTree({
              productType: props.info?.taskTypeEnum,
              structureClassId: v.configId,
              disabledIds: pIds.value || [],
              _factoryIds: (v.factoryId && [v.factoryId]) || []
            })
          }
          classIdGroupsObj.value[v.configId] = res
        }
      }
      tableData.value.push(v)
    }
  } catch (error) {
    console.log('可变更的任务工单详情列表获取失败', error)
  } finally {
    tableLoading.value = false
  }
}

function detailSuccess() {
  fetch()
  emit('success')
}

function handleSuccess() {
  fetch()
}

function deal(row) {
  detailData.value = {}
  detailVisible.value = true
  detailData.value = row
  detailList.value = row?.mesBuildingAssistDetailDTOS || []
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
