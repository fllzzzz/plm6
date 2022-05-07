<template>
  <common-drawer
    ref="drawerRef"
    :title="`${details.serialNumber}-协同任务信息`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="30%"
  >
    <template #titleRight>
      <common-button
        v-permission="assistPermission.edit"
        v-show="!isEdit"
        size="mini"
        type="primary"
        :disabled="!unCompleteQuantity"
        @click="toEdit"
      >
        添加
      </common-button>
      <common-button v-show="isEdit" size="mini" :loading="submitLoading" type="success" :disabled="!canSubmit" @click="submitIt">
        保存
      </common-button>
      <common-button v-show="isEdit" size="mini" type="warning" @click="cancelEdit">取消编辑</common-button>
    </template>
    <template #content>
      <el-form size="small" label-position="left" label-width="70px">
        <el-form-item label="所属项目">
          <span>{{ details.project?.name }}</span>
        </el-form-item>
        <el-form-item label="任务数">
          <span>{{ details.sourceSchedulingQuantity }}</span>
        </el-form-item>
        <el-form-item label="未完成数">
          <span>{{ unCompleteQuantity }}</span>
        </el-form-item>
      </el-form>
      <common-table v-loading="tableLoading" :data="list" return-source-data style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="productionLineName" :show-overflow-tooltip="true" label="生产线">
          <template v-slot="scope">
            <span>{{ scope.row.productionLineName }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="processName" :show-overflow-tooltip="true" label="班组">
          <template v-slot="scope">
            <span>{{ scope.row.processName }} - {{ scope.row.leaderName }}</span>
          </template>
        </el-table-column>
        <el-table-column v-permission="assistPermission.del" label="操作" width="90px" align="center">
          <template v-slot="scope">
            <el-popconfirm
              confirm-button-text="确定"
              cancel-button-text="取消"
              title="确定删除该协同任务吗？"
              @confirm="confirmEvent(scope.row)"
            >
              <template #reference>
                <common-button type="danger" style="padding: 6px" icon="el-icon-delete" size="mini" />
              </template>
            </el-popconfirm>
          </template>
        </el-table-column>
      </common-table>
      <common-table v-show="isEdit" :data="addList" return-source-data style="width: 100%; margin-top: 10px">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="productionLineName" :show-overflow-tooltip="true" label="生产线">
          <template v-slot="scope">
            <common-select
              v-model="scope.row.productionLineId"
              :options="lines"
              type="other"
              :loading="teamListLoading"
              size="mini"
              clearable
              placeholder="请选择生产线"
              style="width: 150px"
            />
          </template>
        </el-table-column>
        <el-table-column prop="processName" :show-overflow-tooltip="true" label="班组">
          <template v-slot="scope">
            <common-select
              v-model="scope.row.teamId"
              :options="linesMap[scope.row.productionLineId]?.teamList || []"
              :disabledVal="[...originDisabledTeamId, ...disabledTeamId]"
              type="other"
              size="mini"
              clearable
              :noDataText="scope.row.productionLineId ? '暂无数据' : '未选择生产线'"
              placeholder="请选择班组"
              style="width: 150px; margin-left: 5px"
            >
            </common-select>
          </template>
        </el-table-column>
        <el-table-column label="操作" width="90px" align="center">
          <template v-slot="scope">
            <common-button
              size="mini"
              v-if="scope.$index === addList.length - 1"
              type="primary"
              icon="el-icon-plus"
              @click="add(scope.$index)"
              style="padding: 6px"
            ></common-button>
            <common-button
              v-if="addList.length !== 1"
              size="mini"
              type="danger"
              icon="el-icon-delete"
              @click="del(scope.$index)"
              style="padding: 6px"
            ></common-button>
          </template>
        </el-table-column>
      </common-table>
      <!-- <assistance-task v-model:visible="assistanceTaskVisible" :details="details" @success="fetchList" /> -->
    </template>
  </common-drawer>
</template>

<script setup>
import crudApi, { teamList } from '@/api/mes/scheduling-manage/task/assistance'
import { defineProps, defineEmits, ref, watch, computed, reactive, inject } from 'vue'
import { ElNotification } from 'element-plus'

import { deepClone } from '@data-type/index'

import useVisible from '@compos/use-visible'
// import assistanceTask from './assistance-task'

const assistPermission = inject('assistPermission')

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  details: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

const unCompleteQuantity = computed(() => {
  return (props.details?.sourceSchedulingQuantity || 0) - (props.details?.completeQuantity || 0)
})

const originDisabledTeamId = computed(() => {
  return list.value.map((v) => v.teamId)
})

const disabledTeamId = computed(() => {
  return addList.value.map((v) => v.teamId)
})

const isEdit = ref(false)
const teamListLoading = ref(false)
const lines = ref([])
const linesMap = reactive({})
const taskId = ref()
async function fetchTeamList() {
  try {
    teamListLoading.value = true
    const { id, productionLineList } = await teamList({
      productType: props.details?.productType,
      schedulingId: props.details?.id
    })
    productionLineList.forEach((v) => {
      v.teamList = v.teamList.map((o) => {
        o.name = o.processName + ' - ' + o.leaderName
        return o
      })
      linesMap[v.id] = v
    })
    lines.value = productionLineList.filter((v) => v.teamList.length)
    taskId.value = id
  } catch (error) {
    console.log('获取可协同生产线-班组列表', error)
  } finally {
    teamListLoading.value = false
  }
}

// const assistanceTaskVisible = ref(false)
const tableLoading = ref(false)
const list = ref([])
const originList = ref([])
const addList = ref([])

const canSubmit = computed(() => {
  return addList.value.filter((v) => v.productionLineId && v.teamId)?.length
})

async function fetchList() {
  cancelEdit()
  try {
    tableLoading.value = true
    const content = await crudApi.get({
      schedulingId: props.details?.id
    })
    originList.value = deepClone(content)
    list.value = content || []
  } catch (error) {
    console.log('获取协同任务列表失败')
  } finally {
    tableLoading.value = false
  }
}

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchTeamList()
      fetchList()
    } else {
      cancelEdit()
    }
  },
  { immediate: true }
)

function toEdit() {
  isEdit.value = true
  add()
}

function cancelEdit() {
  isEdit.value = false
  addList.value = []
}

function add() {
  addList.value.push({})
}

function del(index) {
  addList.value.splice(index, 1)
}

async function confirmEvent(row) {
  try {
    await crudApi.del({ ids: [row.id] })
    ElNotification({ title: '删除成功', type: 'success', duration: 2500 })
    fetchList()
  } catch (error) {
    console.log('删除协同任务失败')
  }
}
const submitLoading = ref(false)
async function submitIt() {
  try {
    submitLoading.value = true
    const assistList = addList.value
      .filter((v) => v.productionLineId && v.teamId)
      .map((v) => {
        return {
          taskId: taskId.value,
          teamId: v.teamId
        }
      })
    await crudApi.add({
      assistList
    })
    ElNotification({ title: '添加协同任务成功', type: 'success', duration: 2500 })
    fetchList()
  } catch (error) {
    console.log('添加协同任务失败')
  } finally {
    submitLoading.value = false
  }
}
</script>
