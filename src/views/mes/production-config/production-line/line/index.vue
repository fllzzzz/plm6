<template>
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
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      highlight-current-row
      :data="crud.data"
      :empty-text="crud.emptyText"
      :data-format="dataFormat"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
      style="width: 100%"
      @current-change="handleCurrentChange"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
        <template #default="{ row: { sourceRow: row } }">
          <p>
            工厂：<span style="margin-right: 20px">{{ row.factoryName }}</span> 车间：<span>{{ row.workshopName }}</span>
          </p>
          <template v-if="!(row.productType & (componentTypeEnum.ENCLOSURE.V | componentTypeEnum.MACHINE_PART.V))">
            <p v-if="row.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V && row.productType & componentTypeEnum.ARTIFACT.V">
              产品标识：<span>{{ row.typeSequence }}</span>
            </p>
            <p v-else>
              可生产类型：<span>{{ row.typeSequence }}</span>
            </p>
          </template>
          <p>
            备注：<span>{{ row.remark }}</span>
          </p>
        </template>
      </el-expand-table-column>
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        :show-overflow-tooltip="true"
        label="生产线名称"
        min-width="140px"
      >
        <template #default="{ row }">
          <table-cell-tag
            v-if="row.productType"
            :name="componentTypeEnum.V[row.productType].SL"
            :color="componentTypeEnum.V[row.productType].COLOR"
            :offset="15"
          />
          <span>{{ row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('productionLineTypeEnum')"
        prop="productionLineTypeEnum"
        :show-overflow-tooltip="true"
        label="生产线类型"
        align="center"
        width="100px"
      >
        <template v-slot="{ row }">
          <span>{{ artifactProductLineEnum.VL[row.productionLineTypeEnum] }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('targetProductionShow')"
        key="targetProductionShow"
        prop="targetProductionShow"
        :show-overflow-tooltip="true"
        label="目标产量(吨/月)"
        align="center"
        width="110px"
      >
        <template v-slot="{ row }">
          <span>{{ row.targetProductionShow }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column
        v-if="columns.visible('shortName')"
        key="shortName"
        prop="shortName"
        :show-overflow-tooltip="true"
        label="生产线简称"
        min-width="140px"
      >
        <template #default="{ row }">
          <span>{{ row.shortName }}</span>
        </template>
      </el-table-column> -->
      <el-table-column
        v-if="columns.visible('boolEnabledEnum')"
        key="boolEnabledEnum"
        prop="boolEnabledEnum"
        label="状态"
        align="center"
        width="80px"
      >
        <template v-slot:header>
          <el-tooltip class="item" effect="light" :content="`生产线被禁用后，该生产线无法再在其他页面中显示`" placement="top">
            <div style="display: inline-block">
              <span>状态</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <el-switch
            v-model="scope.row.boolEnabledEnum"
            :disabled="!checkPermission(permission.editStatus)"
            active-color="#409EFF"
            inactive-color="#F56C6C"
            :active-value="enabledEnum.TRUE.V"
            :inactive-value="enabledEnum.FALSE.V"
            @change="changeStatus(scope.row, scope.row.boolEnabledEnum)"
          />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('sort')" key="sort" prop="sort" label="排序" align="center" width="80px" />
      <el-table-column
        v-if="checkPermission([...permission.del, ...permission.edit])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <udOperation :data="scope.row" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </el-card>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/mes/production-config/production-line'
import { ref, defineEmits, inject } from 'vue'
import { useStore } from 'vuex'
import { enabledEnum } from '@enum-ms/common'
import { componentTypeEnum, artifactProductLineEnum } from '@enum-ms/mes'
import checkPermission from '@/utils/system/check-permission'
import { configProductionLinePM as permission } from '@/page-permission/config'

import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import { ElMessageBox } from 'element-plus'

const store = useStore()
const emit = defineEmits(['click-line'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '生产线',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['shortName', 'factoryName', 'workshopName']
  },
  tableRef
)
// 展开keys
const expandRowKeys = ref([])
const dataFormat = ref([
  ['targetProductionShow', ['to-fixed', 2]]
])

const maxHeight = inject('maxHeight')

async function changeStatus(data, val) {
  try {
    await ElMessageBox.confirm('此操作将 "' + enabledEnum.VL[val] + '" ' + data.name + ', 是否继续？', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus({ id: data.id, boolEnabledEnum: val })
    crud.refresh()
    crud.notify(enabledEnum.VL[val] + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
    changeStoreLoaded()
  } catch (error) {
    console.log('变更生产线状态', error)
    data.boolEnabledEnum = data.boolEnabledEnum === enabledEnum.TRUE.V ? enabledEnum.FALSE.V : enabledEnum.TRUE.V
  }
}

function handleCurrentChange(val) {
  if (val) {
    emit('click-line', val)
  }
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach((v) => {
    v.targetProductionShow = (v.targetProduction && v.targetProduction / 1000) || 0
    v.typeSequence = v?.typeList?.map((v) => `【${v?.name}】`)?.join('')
    v.linkIdList = v?.typeList?.map((v) => v.id) || []
  })
}

// 编辑之后 取消缓存的已加载设置
CRUD.HOOK.afterSubmit = () => {
  changeStoreLoaded()
}
CRUD.HOOK.afterDelete = () => {
  changeStoreLoaded()
}
function changeStoreLoaded() {
  store.commit('config/SET_LOADED', { key: 'productLines', loaded: false })
  store.commit('config/SET_LOADED', { key: 'onlyProductLines', loaded: false })
}
</script>

<style lang="scss" scoped>
::v-deep(.line-box) {
  .el-card__body {
    padding-top: 11px;
    .el-tabs {
      margin-bottom: 7px;
    }
  }
  .card-header {
    height: 28px;
  }
}
</style>
