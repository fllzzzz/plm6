<template>
  <el-drawer
    title="区域详情"
    v-model:visible="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="50%"
  >
    <div class="el-drawer-container">
      <el-table
        ref="table"
        v-loading="loading"
        :border="$TBS.BORDER"
        :data="list"
        :max-height="$_height"
        style="width: 100%;"
      >
        <el-table-column v-if="$TBS.INDEX" :label="$TBS.INDEX_LABEL ? '序号': ''" type="index" align="center" width="60" />
        <el-table-column :show-overflow-tooltip="true" prop="name" label="区域" />
        <el-table-column :show-overflow-tooltip="true" prop="axis" label="轴线/标高" />
        <el-table-column prop="quantity" label="合计数量" align="center" />
        <template v-if="materialListType === typeEnum.ENCLOSURE.V">
          <el-table-column prop="mete" :label="`合计量 (m)`" align="center">
            <template v-slot="scope">
              <span>{{ scope.row.mete | convertUnit('mm', 'm', $DP.MES_ENCLOSURE_L__M) }}</span>
            </template>
          </el-table-column>
        </template>
        <template v-else>
          <el-table-column prop="grossMete" :label="`合计毛重 (t)`" align="center">
            <template v-slot="scope">
              <span>{{ scope.row.grossMete | convertUnit('kg', 't', $DP.COM_WT__T) }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="netMete" :label="`合计净重 (t)`" align="center">
            <template v-slot="scope">
              <span>{{ scope.row.netMete | convertUnit('kg', 't', $DP.COM_WT__T) }}</span>
            </template>
          </el-table-column>
        </template>
      </el-table>
    </div>
  </el-drawer>
</template>

<script>
import { getSummaryListForArea as getSummary } from '@/api/mes-plan/technical-manage/common'
import sizeCalc from '@/mixins/sizeCalc'
import { materialListTypeEnum as typeEnum } from '@/utils/enum/index'
import checkPermission from '@/utils/system/check-permission'
import { debounce } from '@/utils'

// crud交由presenter持有
const permission = {
  get: ['materialListSummary:get']
}

export default {
  mixins: [sizeCalc],
  props: {
    visible: {
      type: Boolean,
      required: true
    },
    materialListType: {
      type: Number,
      default: typeEnum.ARTIFACT.V
    },
    monomerId: {
      type: [Number, String],
      default: undefined
    }
  },
  data() {
    return {
      typeEnum,
      extraHeight: 80,
      loading: false,
      list: []
    }
  },
  computed: {
    drawerVisible() {
      return this.visible
    },
    unitInfo() {
      const data = {}
      if (this.materialListType === typeEnum.ARTIFACT.V || this.materialListType === typeEnum.MACHINE_PART.V) {
        data.decimal = this.$DP.COM_WT__T
        data.from = 'kg'
        data.to = 't'
      }
      if (this.materialListType === typeEnum.ENCLOSURE.V) {
        data.decimal = this.$DP.MES_ENCLOSURE_L__M
        data.from = 'mm'
        data.to = 'm'
      }
      return data
    }
  },
  watch: {
    monomerId(val) {
      this.fetchSummary()
    },
    materialListType(val) {
      this.fetchSummary()
    }
  },
  created() {
    this.fetchSummary()
  },
  methods: {
    handleClose() {
      this.$emit('update:visible', false)
    },
    fetchSummary: debounce(async function () {
      if (!checkPermission(permission.get) || !this.monomerId) {
        return
      }
      this.loading = true
      let list = []
      try {
        const params = {
          type: this.materialListType,
          monomerId: this.monomerId
        }
        const { content = [] } = await getSummary(params)
        list = content
      } catch (error) {
        console.log('获取区域清单汇总', error)
      } finally {
        this.list = list
        this.loading = false
      }
    }, 200, false)
  }
}
</script>

<style lang="scss" scoped>
/deep/.el-drawer__header {
    margin-bottom: 10px;
}
.el-drawer-container {
  padding: 0 20px 20px 20px;
  overflow: auto;
}
</style>
