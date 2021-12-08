<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader :project-id="globalProjectId" />
    </div>
    <!--表格渲染-->
    <el-table
      ref="table"
      v-loading="crud.loading"
      :border="$TBS.BORDER"
      :stripe="$TBS.STRIPE"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="$_tableMaxHeight({paginate: false})"
      style="width: 100%;"
    >
      <el-table-column v-if="$TBS.INDEX" :label="$TBS.INDEX_LABEL ? '序号': ''" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="单体" width="200px" />
      <el-table-column v-if="columns.visible('quantity')" key="quantity" prop="quantity" label="合计数量" align="center" width="200px" />
      <template v-if="crud.query.type === typeEnum.ENCLOSURE.V">
        <el-table-column v-if="columns.visible('mete')" key="mete" prop="mete" :label="`合计量 (m)`" align="center" width="200px">
          <template v-slot="scope">
            <span>{{ scope.row.mete | convertUnit('mm','m') | toFixed($DP.MES_ENCLOSURE_L__M) }}</span>
          </template>
        </el-table-column>
      </template>
      <template v-else>
        <el-table-column v-if="columns.visible('grossMete')" key="grossMete" prop="grossMete" :label="`合计毛重 (t)`" align="center" width="200px">
          <template v-slot="scope">
            <span>{{ scope.row.grossMete | convertUnit('kg', 't', $DP.COM_WT__T) }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('netMete')" key="netMete" prop="netMete" :label="`合计净重 (t)`" align="center" width="200px">
          <template v-slot="scope">
            <span>{{ scope.row.netMete | convertUnit('kg', 't', $DP.COM_WT__T) }}</span>
          </template>
        </el-table-column>
      </template>
      <el-table-column
        v-permission="permission.get"
        label="操作"
        align="left"
      >
        <template v-slot="scope">
          <el-button size="small" type="primary" @click="openAreaList(scope.row)">查看区域详情</el-button>
        </template>
      </el-table-column>
    </el-table>
    <area-list :visible.sync="areaListVisible" :monomer-id="currentRow.id" :material-list-type="crud.query.type" />
  </div>
</template>

<script>
import { mapGetters } from 'vuex'
import { getSummaryListForMonomer as get } from '@/api/mes-plan/technical-manage/common'
import CRUD, { presenter } from '@crud/crud'
import mHeader from './module/header'
import areaList from './module/area-list'
import { materialListTypeEnum as typeEnum } from '@/utils/enum/index'

// crud交由presenter持有
const permission = {
  get: ['materialListSummary:get']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const crud = CRUD({
  title: '清单汇总',
  sort: ['sort.asc', 'id.desc'],
  optShow: { ...optShow },
  permission: { ...permission },
  requiredQuery: ['projectId'],
  crudMethod: { get },
  hasPagination: false
})

export default {
  name: 'MesSummaryList',
  components: { mHeader, areaList },
  mixins: [presenter(crud)],
  inject: ['$_tableMaxHeight'],
  data() {
    return {
      typeEnum,
      permission,
      areaListVisible: false,
      currentRow: {}
    }
  },
  computed: {
    ...mapGetters([
      'globalProjectId'
    ]),
    unitInfo() {
      const data = {}
      if (this.crud.query.type === typeEnum.ARTIFACT.V || this.crud.query.type === typeEnum.MACHINE_PART.V) {
        data.decimal = this.$DP.COM_WT__T
        data.from = 'kg'
        data.to = 't'
      }
      if (this.crud.query.type === typeEnum.ENCLOSURE.V) {
        data.decimal = this.$DP.MES_ENCLOSURE_L__M
        data.from = 'mm'
        data.to = 'm'
      }
      return data
    }
  },
  methods: {
    handleAreaDrawerClose() {
      this.areaListVisible = false
    },
    openAreaList(row) {
      this.currentRow = row
      this.areaListVisible = true
    }
  }
}
</script>
