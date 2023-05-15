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
          :style="{ maxHeight: maxHeight + 20 + 'px' }"
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
        <mHeader>
          <template #viewLeft>
            <el-tag v-if="treeRow?.levelName" size="medium" effect="plain" type="warning" class="filter-item">
              {{ treeRow?.levelName }}
            </el-tag>
            <div class="btn-wrap">
              <common-button v-permission="permission.auto" type="primary" size="mini" @click="autoAmortizationVisible = true">
                自动摊销
              </common-button>
              <el-badge
                :value="manualAmortizationCount"
                :max="99"
                :style="{ marginRight: manualAmortizationCount ? '10px' : 0 }"
                :hidden="manualAmortizationCount < 1"
              >
                <common-button v-permission="permission.manual" type="primary" size="mini" @click="manualAmortizationVisible = true">
                  手动摊销
                </common-button>
              </el-badge>
              <common-button v-permission="permission.set" type="warning" size="mini" @click="amortizationSettingVisible = true">
                摊销设置
              </common-button>
            </div>
          </template>
        </mHeader>
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          :empty-text="crud.emptyText"
          :max-height="maxHeight"
          row-key="id"
          :data-format="columnsDataFormat"
        >
          <el-table-column type="index" prop="index" label="序号" align="center" width="60px" />
          <el-table-column v-if="columns.visible('date')" prop="date" key="date" label="摊销时间段" align="center" min-width="160">
            <template #default="{ row }">
              <span style="font-weight: bold">{{ row.date }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('name')"
            align="center"
            key="name"
            prop="name"
            :show-overflow-tooltip="true"
            label="摊销种类"
            min-width="180"
          >
            <template #default="{ row }">
              <span>
                <span v-if="row.fullPathName" style="color: #adadad">{{ row.fullPathName }} > </span>{{ row.name }}
              </span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('amount')"
            align="center"
            key="amount"
            prop="amount"
            :show-overflow-tooltip="true"
            label="摊销金额"
            min-width="120"
          />
          <el-table-column
            v-if="columns.visible('productMete')"
            align="center"
            key="productMete"
            prop="productMete"
            :show-overflow-tooltip="true"
            label="产量（吨）"
            min-width="120"
          />
          <el-table-column
            v-if="columns.visible('amortizationType')"
            align="center"
            key="amortizationType"
            prop="amortizationType"
            :show-overflow-tooltip="true"
            label="摊销类型"
            min-width="100"
          />
          <el-table-column v-if="checkPermission(permission.detail)" align="center" label="操作" width="80px">
            <template #default="{ row }">
              <common-button size="mini" type="info" icon="el-icon-view" @click="showDetail(row)" />
            </template>
          </el-table-column>
        </common-table>
        <!-- 分页 -->
        <pagination />
        <amortization-setting v-model="amortizationSettingVisible" @success="getAmortizationTree" />
        <auto-amortization v-model="autoAmortizationVisible" />
        <manual-amortization v-model="manualAmortizationVisible" @success="crud.toQuery" />
        <mDetail v-model="detailVisible" :detail-row="detailRow" />
      </div>
    </div>
  </div>
</template>

<script setup>
import crudApi, { amortizationClassTree, getManualAmortizationCount } from '@/api/contract/expense-entry/amortization-manage'
import { ref, provide } from 'vue'

import { amortizationManagePM as permission } from '@/page-permission/contract'
import { setEmptyArr2Undefined, setLevelName } from '@/utils/data-type/tree'
import moment from 'moment'
import checkPermission from '@/utils/system/check-permission'
import { amortizationTypeEnum } from '@enum-ms/contract'

import pagination from '@crud/Pagination'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import mHeader from './module/header.vue'
import mDetail from './module/detail.vue'
import amortizationSetting from './module/amortization-setting'
import autoAmortization from './module/auto-amortization'
import manualAmortization from './module/manual-amortization'
import useMatClsList from '@/composables/store/use-mat-class-list'

const tableRef = ref()
const amortizationTreeRef = ref()
const amortizationTree = ref([])
const amortizationKV = ref({})
const amortizationClassEnumKV = ref({})
const treeRow = ref({})
const detailRow = ref({})
const detailVisible = ref(false)
const amortizationSettingVisible = ref(false)
const autoAmortizationVisible = ref(false)
const manualAmortizationVisible = ref(false)
const manualAmortizationCount = ref(0)

const defaultProps = ref({
  children: 'children',
  label: 'name'
})

const columnsDataFormat = ref([
  ['avgUnitPrice', 'to-thousand'],
  ['productMete', 'to-thousand'],
  ['amortizationType', ['parse-enum', amortizationTypeEnum]]
])

provide('amortizationClassEnumKV', amortizationClassEnumKV)
const { rawMatClsKV } = useMatClsList()

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

const { maxHeight } = useMaxHeight({
  paginate: true
})

getAmortizationTree()

async function getAmortizationTree() {
  try {
    amortizationKV.value = {}
    amortizationClassEnumKV.value = {}
    amortizationTree.value = (await amortizationClassTree({ enable: true })) || []
    setLevelName(amortizationTree.value)
    setAmortizationKV(amortizationTree.value)
    setEmptyArr2Undefined(amortizationTree.value)
    nodeClick(treeRow.value)
  } catch (e) {
    console.log('获取摊销种类失败', e)
  }
}

// 设置摊销KV
function setAmortizationKV(tree) {
  tree?.forEach((row) => {
    amortizationKV.value[row.id] = row
    if (row.bizId === 0) {
      amortizationClassEnumKV.value[row.amortizationClassEnum] = row
    }
    setAmortizationKV(row.children)
  })
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
  if (row.id && row.id === treeRow.value.id) {
    row = {}
  }
  treeRow.value = row
  amortizationTreeRef.value.setCurrentKey(row.id)
  crud.query.ids = []
  if (row.id) {
    getIds([row])
  }
  crud.toQuery()
}

function showDetail(row) {
  detailRow.value = row
  detailVisible.value = true
}

async function getCount() {
  try {
    manualAmortizationCount.value = (await getManualAmortizationCount()) || 0
  } catch (e) {
    console.log('获取手动待摊销数量', e)
  }
}

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  getCount()
  data.content.forEach((row) => {
    // 时间范围
    const _startDate = moment(row.startDate).format('YYYY-MM-DD')
    const _endDate = moment(row.endDate).format('YYYY-MM-DD')
    row.date = `${_startDate} ~ ${_endDate}`
    row.bizId = amortizationKV.value[row.amortizationClassId]?.bizId
    // 科目层级名称
    const raw = rawMatClsKV.value[row.bizId]
    if (raw) {
      const names = [raw.basicClassName, ...raw.fullPathName]
      row.name = names.at(-1)
      names.splice(names.length - 1)
      row.fullPathName = names.join(' > ')
    }
  })
}
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

.btn-wrap {
  > * {
    margin-left: 6px;
    vertical-align: middle;
  }
}
</style>
