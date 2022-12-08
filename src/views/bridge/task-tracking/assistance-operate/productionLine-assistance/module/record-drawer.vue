<template>
  <common-drawer
    customClass="assistance-record"
    ref="drawerRef"
    title="协同记录"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="100%"
  >
    <template #titleAfter>
      <common-radio-button
        v-model="query.taskTypeEnum"
        :options="queryTaskTypeENUM"
        type="enum"
        default
        class="filter-item"
        @change="fetch"
      />
    </template>
    <template #content>
      <div style="display: flex; height: 100%">
        <div style="width: 45%">
          <common-table
            v-loading="recordLoading"
            highlight-current-row
            :stripe="false"
            @current-change="handleClickChange"
            :data="recordList"
            :data-format="dataFormat"
            :max-height="maxHeight"
            style="width: 100%"
          >
            <el-table-column label="序号" type="index" align="center" width="60" />
            <el-table-column align="center" prop="orderNumber" :show-overflow-tooltip="true" label="排产工单号" min-width="120px" />
            <el-table-column prop="createTime" :show-overflow-tooltip="true" label="日期" width="100px" align="center" />
            <el-table-column prop="user.name" :show-overflow-tooltip="true" label="协同操作人" width="100px" align="center" />
            <!-- <el-table-column prop="taskTypeEnum" :show-overflow-tooltip="true" label="属性" width="90px" align="center" /> -->
            <el-table-column
              v-if="query.taskTypeEnum !== taskTypeENUM.MACHINE_PART.V"
              align="center"
              prop="monomer.name"
              :show-overflow-tooltip="true"
              label="单体"
              width="100px"
            />
            <el-table-column prop="quantity" :show-overflow-tooltip="true" label="协同数（件/kg）" width="120px" align="center">
              <template #default="{ row }">
                <span>{{ row.quantity }} / {{ row.totalNetWeight }}</span>
              </template>
            </el-table-column>
            <el-table-column
              v-if="query.taskTypeEnum !== taskTypeENUM.MACHINE_PART.V"
              prop="group.name"
              :show-overflow-tooltip="true"
              label="原生产组"
              min-width="150px"
            >
              <template #default="{ row }">
                <span>{{ row.workshop?.name }}>{{ row.productionLine?.name }}>{{ row.groups?.name }}</span>
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
        </div>
        <el-divider direction="vertical" style="height: 100%"></el-divider>
        <div style="flex: 1">
          <el-tag v-if="!detailList?.length" type="info" size="medium"> * 请先选择协同记录查看协同详情 </el-tag>
          <template v-else>
            <common-table v-loading="detailLoading" :data="detailList" :max-height="maxHeight" style="width: 100%">
              <el-table-column label="序号" type="index" align="center" width="60" />
              <el-table-column
                v-if="currentInfo.taskTypeEnum !== taskTypeENUM.MACHINE_PART.V"
                align="center"
                prop="orderNumber"
                :show-overflow-tooltip="true"
                label="排产工单号"
                min-width="120px"
              />
              <el-table-column
                v-if="currentInfo.taskTypeEnum === taskTypeENUM.MACHINE_PART.V"
                align="center"
                prop="cutNumber"
                :show-overflow-tooltip="true"
                label="切割指令号"
                min-width="120px"
              />
              <el-table-column
                v-if="currentInfo.taskTypeEnum !== taskTypeENUM.MACHINE_PART.V"
                prop="area.name"
                :show-overflow-tooltip="true"
                label="区域"
                width="100px"
                align="center"
              />
              <el-table-column
                v-if="currentInfo.taskTypeEnum !== taskTypeENUM.MACHINE_PART.V"
                prop="serialNumber"
                :show-overflow-tooltip="true"
                label="编号"
                width="100px"
                align="center"
              />
              <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="100px" align="center" />
              <el-table-column
                v-if="currentInfo.taskTypeEnum !== taskTypeENUM.MACHINE_PART.V"
                prop="length"
                :show-overflow-tooltip="true"
                label="长度(mm)"
                width="80"
                align="center"
              />
              <el-table-column prop="quantity" :show-overflow-tooltip="true" label="协同数量" width="80" align="center" />
              <el-table-column prop="groups?.name" :show-overflow-tooltip="true" label="协同生产组" min-width="150">
                <template #default="{ row }">
                  <span>{{ row.workshop?.name }}>{{ row.productionLine?.name }}>{{ row.groups?.name }}</span>
                </template>
              </el-table-column>
            </common-table>
          </template>
        </div>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { record, recordDetail } from '@/api/bridge/bridge-task-tracking/assistance-operate/productionLine-assistance'
import { defineProps, defineEmits, ref } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
import { taskTypeENUM } from '@enum-ms/mes'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetch })

// 高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.assistance-record',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const queryTaskTypeENUM = {
  ARTIFACT: taskTypeENUM.ARTIFACT,
  ASSEMBLE: { L: '部件', K: 'ASSEMBLE', V: taskTypeENUM.ASSEMBLE.V | taskTypeENUM.PARENT_PART.V },
  MACHINE_PART: taskTypeENUM.MACHINE_PART
}

const dataFormat = ref([
  ['createTime', ['parse-time', '{y}-{m}-{d}']],
  ['taskTypeEnum', ['parse-enum', taskTypeENUM]]
])

function showHook() {
  detailList.value = []
  fetch()
}
const recordLoading = ref(false)
const recordList = ref([])
const detailLoading = ref(false)
const detailList = ref([])
const query = ref({})
const currentInfo = ref({})

async function fetch() {
  try {
    recordLoading.value = true
    recordList.value = []
    detailList.value = []
    const { content, totalElements } = await record({ ...query.value, ...queryPage })
    recordList.value = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取协同记录失败', error)
  } finally {
    recordLoading.value = false
  }
}

async function detail(row) {
  if (!row || !row.id) return
  try {
    detailLoading.value = true
    const content = await recordDetail({ changeId: row.id })
    detailList.value = content
  } catch (error) {
    detailList.value = []
    console.log('获取协同记录详情失败', error)
  } finally {
    detailLoading.value = false
  }
}

function handleClickChange(val) {
  currentInfo.value = val
  detail(val)
}
</script>
