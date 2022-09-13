<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="header" :permission="permission" >
      <template #layConfig>
        <common-button @click="unLoading" type="primary" size="mini">下料方式配置</common-button>
      </template>
    </mHeader>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :show-empty-symbol="false"
      :max-height="maxHeight"
      style="width: 100%"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('layingOffWay')" align="center" key="layingOffWay" prop="layingOffWay" label="下料方式">
        <template #default="{ row }">
          <span> {{ row.layingOffWay }} </span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('cutType')" :show-overflow-tooltip="true" prop="cutType" label="切割形式" align="center">
        <template #default="{ row }">
          <span> {{ row.cutType }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('thickness')" :show-overflow-tooltip="true" prop="thickness" label="厚度" align="center">
        <template #default="{ row }">
          <span>{{ row.thickness }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('boolDrillEnum')"
        :show-overflow-tooltip="true"
        prop="boolDrillEnum"
        label="是否切孔"
        align="center"
      >
        <template #default="{ row }">
          <template v-if="checkPermission(permission.edit)">
            <el-switch
              :disabled="row.enabledLoading"
              v-model="row.boolDrillEnum"
              class="drawer-switch"
              @change="handleEnabledChange(row, ['cutTypeLabel'])"
            />
          </template>
          <template v-else>
            {{ whetherEnum.VL[row.boolDrillEnum] }}
          </template>
        </template>
      </el-table-column>
      <!-- <el-table-column v-if="columns.visible('updateTime')" key="updateTime" prop="updateTime" label="编辑日期" width="140px" />
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建日期" width="140px" /> -->
      <!--编辑与删除-->
      <el-table-column v-permission="permission.del" label="操作" width="130px" align="center">
        <template #default="{ row }">
          <udOperation :show-edit="false" :data="row" />
        </template>
      </el-table-column>
    </common-table>
    <m-batch-form :detail-data="detailData" />
    <m-unloading-form v-model="visible.batchUnloadingAdd" :detail-data="detailData" @success="handleAddSuccess" />
  </div>
</template>

<script setup>
import crudApi, { editHole } from '@/api/mes/production-config/cutting-config'
import { mesCuttingConfigPM as permission } from '@/page-permission/config'
import { batchUnloading } from '@/api/mes/production-config/unloading-config'
import { ref, reactive } from 'vue'
import { whetherEnum } from '@enum-ms/common'
import { baseTimeColumns } from '@/utils/columns-format/common'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useCrudEnabledChange from '@compos/use-crud-enabled-change'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mBatchForm from './module/batch-form'
import mUnloadingForm from './module/unloading-form'

const optShow = {
  batchAdd: true,
  add: false,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const visible = reactive({
  batchUnloadingAdd: false
})
const detailData = ref([])
// 表格列数据格式转换
const columnsDataFormat = ref([...baseTimeColumns])
const { CRUD, crud, columns } = useCRUD(
  {
    title: '切割配置',
    hasPagination: false,
    formStore: true,
    formStoreKey: 'MES_CUTTING_CONFIG',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ extraBox: ['.unloading-mode'], paginate: false })

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content = data.content.map((v) => {
    v.cutTypeLabel = `“${v.layingOffWay}-${v.cutType}-${v.thickness}”的切孔改为：`
    return v
  })
}

// 启用状态变更
const { handleEnabledChange } = useCrudEnabledChange(
  { CRUD, crud, editEnabled: editHole },
  { enabledField: 'boolDrillEnum', enumObj: whetherEnum }
)

// 下料方式配置
function unLoading() {
  visible.batchUnloadingAdd = true
}

fetchList()
// 查询下料方式
async function fetchList() {
  try {
    const data = await batchUnloading()
    detailData.value = data
  } catch (error) {
    console.log('查询下料方式')
  }
}
function handleAddSuccess() {
  fetchList()
}
</script>
