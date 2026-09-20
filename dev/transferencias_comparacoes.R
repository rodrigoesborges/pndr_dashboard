# DEPRECATED (2026-09-20, fase D5 do roadmap_gastos_tributarios.md) ----------
#
# Este one-off era a REFERENCIA DE METODO dos indicadores de transferencias
# (massa mdata 89 / pop mdata 66 do aedidb, PIB 5938 via RSIDRA +
# extrapolacao >=2022 por fator min(PIB/massa) 2018-2021 x 1.02^t, IGP-M
# ipeadatar, mapas ggplot/gganimate). Nao rodar mais: baixa o portal
# TransfereGov inteiro (transfRgov/download_transferencias_uniao) e le
# credenciais de um host antigo (38.242.154.34).
#
# O metodo agora vive no DW (star schema), nos DOIS bancos (aedidb local e
# painelpndr remoto):
#
#   - tabela transf_uniao_municipio (D2, 2014-2025, origem TransfereGov)
#   - mdata 107 transf_uniao_pc_real   (R$ de 2025, IGP-M)
#   - mdata 108 transf_uniao_pib       (% PIB municipal)
#   - mdata 109 transf_uniao_massa_sal (% massa salarial RAIS)
#   - mdata 110/111 renuncia_fiscal_*  (gastos tributarios, 2022-2025)
#
# Pipeline canonico: coleta/indicadores_transf_renuncias.R (D4; insumos do
# aedidb local, alvo via env D4_ALVO=local|remoto). MAPAS/ANALISES NOVOS
# LEEM O DW (data_values dos mdata acima), nao refazem o download.
# Historico completo do metodo: pndr_coord/roadmap_gastos_tributarios.md.
