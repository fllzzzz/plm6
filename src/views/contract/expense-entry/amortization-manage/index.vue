<template>
  <div class="app-container">
    <div class="flex-r">
      <el-card body-style="padding: 10px" class="amortization-tree">
        <template #header>
          <div>摊销种类</div>
        </template>
        <el-tree
          ref="amortizationTreeRef"
          v-loading="crud.loading"
          :style="{ maxHeight: maxHeight - 20 + 'px' }"
          :data="amortizationTree"
          :props="defaultProps"
          :expand-on-click-node="false"
          node-key="id"
          highlight-current
          default-expand-all
          @node-click="nodeClick"
        />
      </el-card>
      <div style="flex: 1; min-width: 1px">
        <mHeader :row-detail="rowDetail" />
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          :empty-text="crud.emptyText"
          :max-height="maxHeight"
          row-key="id"
          show-summary
          :data-format="columnsDataFormat"
          :summary-method="getSummaries"
        >
          <el-table-column type="index" prop="index" label="序号" align="center" width="60px" />
          <el-table-column v-if="columns.visible('date')" prop="date" key="date" label="摊销时间段" align="center" />
          <el-table-column
            v-if="columns.visible('classifyName')"
            align="center"
            key="classifyName"
            prop="classifyName"
            :show-overflow-tooltip="true"
            label="摊销种类"
          />
          <el-table-column
            v-if="columns.visible('totalAmount')"
            align="center"
            key="totalAmount"
            prop="totalAmount"
            :show-overflow-tooltip="true"
            label="摊销金额"
          />
          <el-table-column
            v-if="columns.visible('usedMete')"
            align="center"
            key="usedMete"
            prop="usedMete"
            :show-overflow-tooltip="true"
            label="产量（吨）"
          />
          <el-table-column v-if="checkPermission([...permission.edit, ...permission.del])" align="center" label="操作" width="140px">
            <template #default="{ row }">
              <udOperation :data="row" :disabled-edit="true" :disabled-del="true" />
            </template>
          </el-table-column>
        </common-table>
        <!-- 分页 -->
        <pagination />
      </div>
    </div>
  </div>
</template>

<script setup>
import crudApi, { amortizationClassTree } from '@/api/contract/expense-entry/amortization-manage'
import { ref, nextTick, provide } from 'vue'

import { gasCostPM as permission } from '@/page-permission/contract'
import { tableSummary } from '@/utils/el-extra'
import { setEmptyArr2Undefined, setLevelName } from '@/utils/data-type/tree'
import { toThousand } from '@/utils/data-type/number'
import moment from 'moment'
import checkPermission from '@/utils/system/check-permission'

import pagination from '@crud/Pagination'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header.vue'

const tableRef = ref()
const amortizationTreeRef = ref()
const amortizationTree = ref([])
const amortizationKV = ref({})
const rowDetail = ref({})

const defaultProps = ref({
  children: 'children',
  label: 'name'
})

const columnsDataFormat = ref([
  ['avgUnitPrice', 'to-thousand'],
  ['usedMete', 'to-thousand']
])

provide('amortizationKV', amortizationKV)

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud, CRUD, columns } = useCRUD(
  {
    title: '摊销管理',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    queryOnPresenterCreated: false
  },
  tableRef
)

getAmortizationTree()

async function getAmortizationTree() {
  try {
    amortizationKV.value = {}
    amortizationTree.value = (await amortizationClassTree()) || []
    setLevelName(amortizationTree.value)
    setAmortizationKV(amortizationTree.value)
    setEmptyArr2Undefined(amortizationTree.value)
    nextTick(() => {
      if (amortizationTree.value.length) {
        selectLast(amortizationTree.value)
      } else {
        crud.toQuery()
      }
    })
  } catch (e) {
    console.log('获取摊销种类失败', e)
  }
}

// 设置摊销KV
function setAmortizationKV(tree) {
  tree?.forEach(row => {
    amortizationKV.value[row.id] = row
    setAmortizationKV(row.children)
  })
}

// 默认选中第一个末级
function selectLast(tree = []) {
  if (tree.length) {
    if (tree[0].children?.length) {
      selectLast(tree[0].children)
    } else {
      nodeClick(tree[0])
    }
  }
}

// 获取摊销种类ids
function getIds(tree = []) {
  tree?.forEach((row) => {
    crud.query.ids.push(row.id)
    getIds(row.children)
  })
}

// el-tree 左键点击
function nodeClick(row = {}) {
  // 取消选中
  if (row.id && row.id === rowDetail.value.id) {
    row = {}
  }
  rowDetail.value = row
  amortizationTreeRef.value.setCurrentKey(row.id)
  crud.query.ids = []
  if (row.id) {
    getIds([row])
  }
  crud.toQuery()
}

// 合计
function getSummaries(param) {
  const data = tableSummary(param, {
    props: ['usedMete', 'totalAmount']
  })
  if (data[3] && data[4]) {
    data[5] = toThousand(data[4] / data[3])
  }
  if (data[3]) {
    data[3] = toThousand(data[3])
  }
  if (data[4]) {
    data[4] = toThousand(data[4])
  }
  return data
}

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  data.content.forEach((v, i) => {
    // 时间范围
    const _startDate = moment(v.startDate).format('YYYY-MM-DD')
    const _endDate = moment(v.endDate).format('YYYY-MM-DD')
    v.date = `${_startDate} ~ ${_endDate}`
  })
}
const { maxHeight } = useMaxHeight({
  paginate: true
})
</script>

<style lang="scss" scoped>
::v-deep(.amortization-tree) {
  width: 340px;
  margin-right: 20px;
  .el-card__header {
    padding: 10px 15px;
    background: #e6e1e1;
  }
  .el-tree {
    overflow-y: auto;
  }
}
</style>
