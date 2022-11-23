<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      highlight-current-row
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      row-key="id"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        :show-overflow-tooltip="true"
        label="切割形式"
        align="center"
        min-width="120px"
      >
      <template #default="{ row }">
         <table-cell-tag
            :name="layOffWayTypeEnum.VL[row.boolNestCutEnum]"
            :color="layOffWayTypeEnum.V[row.boolNestCutEnum].COLOR"
            :offset="15"
          />
          <span>{{ row.name}}</span>
      </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('thickness')"
        key="thickness"
        prop="thickness"
        :show-overflow-tooltip="true"
        label="支持板厚(mm) ≤"
        align="center"
        min-width="120px"
      />
      <el-table-column
        v-if="columns.visible('cuttingHolesJoint')"
        key="cuttingHolesJoint"
        prop="cuttingHolesJoint"
        :show-overflow-tooltip="true"
        label="切孔联割(φ) ≥"
        align="center"
        min-width="120px"
      />
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.edit, ...permission.del])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <udOperation :data="scope.row" :permission="permission" />
        </template>
      </el-table-column>
    </common-table>
    <!-- 表单 -->
    <mForm />
  </div>
</template>
<script setup>
import { ref } from 'vue'
import { useStore } from 'vuex'
import useMaxHeight from '@compos/use-max-height'
import crudApi from '@/api/mes/production-config/cutting-config'
import checkPermission from '@/utils/system/check-permission'
import { configProductionLineGroupPM as permission } from '@/page-permission/config'
import { layOffWayTypeEnum } from '@enum-ms/uploading-form'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header.vue'
import mForm from './module/form'

const store = useStore()
const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}
const tableRef = ref()
const { crud, CRUD, columns } = useCRUD(
  {
    title: '零件下料配置',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: false
})

// 编辑之后 取消缓存的已加载设置
CRUD.HOOK.afterSubmit = () => {
  changeStoreLoaded()
}
CRUD.HOOK.afterDelete = () => {
  changeStoreLoaded()
}
function changeStoreLoaded() {
  store.commit('config/SET_LOADED', { key: 'cutConfigs', loaded: false })
}
</script>
<style lang="scss" scoped>
</style>
