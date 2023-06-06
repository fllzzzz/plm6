<template>
  <div class="app-container">
    <el-row :gutter="10" id="laying-off-content">
      <el-col :xs="24" :sm="24" :md="24" :lg="12" :xl="12" style="margin-bottom: 10px">
        <el-card class="line-box box-card">
          <template v-slot:header>
            <span style="line-height: 28px">{{ crud.title }}列表</span>
            <!-- 新增 -->
            <common-button
              v-permission="permission.add"
              style="float: right; padding: 6px 10px"
              size="mini"
              type="primary"
              icon="el-icon-plus"
              @click="crud.toAdd"
            >
              新增
            </common-button>
          </template>
          <!--表格渲染-->
          <common-table
            ref="tableRef"
            v-loading="crud.loading"
            :data="crud.data"
            highlight-current-row
            :data-format="columnsDataFormat"
            :show-empty-symbol="false"
            :max-height="maxHeight"
            style="width: 100%"
            row-key="id"
            @current-change="handleCurrentChange"
          >
            <el-table-column label="序号" type="index" align="center" width="60" />
            <el-table-column prop="specPrefix" :show-overflow-tooltip="true" label="零件规格前缀" align="center" width="160px" />
            <el-table-column :show-overflow-tooltip="true" label="板厚数值（毫米）" align="center" min-width="200px">
              <template #default="{ row }">
                <span>{{ row.minThickness }}</span>
                <span> ~ </span>
                <span>{{ row.maxThickness }}</span>
              </template>
            </el-table-column>
            <!--编辑与删除-->
            <el-table-column v-permission="permission.del" label="操作" width="130px" align="center">
              <template #default="{ row }">
                <udOperation :show-edit="true" :show-del="true" :data="row" />
              </template>
            </el-table-column>
          </common-table>
          <!-- 表单 -->
          <m-form />
        </el-card>
      </el-col>
      <el-col :xs="24" :sm="24" :md="24" :lg="12" :xl="12" style="margin-bottom: 10px">
        <drill-detail ref="drillDetailRef" @drill-detail="handleChangeCutConfig" :layingOffRow="layingOffRow" @refresh="crud.refresh" />
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import drillDetail from './drill-detail'
import useMaxHeight from '@compos/use-max-height'
import { bridgeConfigStatisticalDrillHolePM as permission } from '@/page-permission/config'
import crudApi from '@/api/bridge/production-config/drill-hole-config'
import { ref, provide } from 'vue'
import { baseTimeColumns } from '@/utils/columns-format/common'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mForm from './module/form.vue'
import { isNotBlank } from '@data-type/index'

const drillDetailRef = ref()
const layingOffRow = ref({})
const cutConfigRow = ref({})

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()

// 表格列数据格式转换
const columnsDataFormat = ref([...baseTimeColumns])
const { crud, CRUD } = useCRUD(
  {
    title: '钻孔配置',
    hasPagination: true,
    formStore: true,
    formStoreKey: 'MES_STATISTICAL_DRILL_HOLE_CONFIG',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    dataPath: ''
  },
  tableRef
)

function handleCurrentChange(val) {
  if (val) {
    layingOffRow.value = val
    cutConfigRow.value = {}
  }
}

function handleChangeCutConfig(val) {
  console.log('handleChangeCutConfig', val)
  if (val) {
    cutConfigRow.value = val
  }
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  if (isNotBlank(layingOffRow.value)) {
    layingOffRow.value = data.data.find((v) => v.id === layingOffRow.value.id)
  }
}
const { maxHeight } = useMaxHeight({
  wrapperBox: ['.app-container', '#laying-off-content'],
  extraBox: ['.head-container', '.el-card__header'],
  paginate: true,
  extraHeight: 55
})
provide('maxHeight', maxHeight)
</script>

